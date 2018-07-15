{-# LANGUAGE OverloadedStrings #-}

module DatabaseOperations where

import Model
import Database.Bolt
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Printf
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe

createMolecule :: T.Text -> T.Text -> BoltActionT IO Molecule 
createMolecule smiles iupacName = do
    [record] <- queryP cypher params
    (I id) <- at record "ID(m)"
    pure $ Molecule id smiles iupacName
  where
    cypher = "create (m:Molecule{ smiles: {s}, iupacName: {i}}) return ID(m)"
    params = M.fromList [("s", T smiles), ("i", T iupacName)]

createCatalyst :: T.Text -> T.Text -> BoltActionT IO Catalyst 
createCatalyst smiles name = do
    [record] <- queryP cypher params
    (I id) <- at record "ID(c)"
    pure $ Catalyst id smiles name
  where
    cypher = "create (c:Catalyst{ smiles: {s}, name: {n}}) return ID(c)"
    params = M.fromList [("s", T smiles), ("n", T name)]

{- напишите функцию, которая умеет принимать реацию на вход и загружать её в
   базу; -}
createReaction 
    :: T.Text 
    -> [(Float, Molecule)] 
    -> [(Float, Molecule)] 
    -> Maybe (Catalyst, Accelerate) 
    -> BoltActionT IO Reaction
createReaction reactionName reagents products catalystAndAccelerate = do
    [record] <- queryP cypher params
    (I id) <- at record "ID(reaction)"
    pure $ Reaction id reactionName
  where
    catalyst = fst <$> catalystAndAccelerate 
    accelerate = snd <$> catalystAndAccelerate 
    cypher = 
        T.concat
            [ "match ", rs0, ", ", ps0, cat0
            , " where ", rs1, " and ", ps1, cat1
            , " create (reaction :Reaction { name: {reactionName} }), "
            , rs2, ", ", ps2, cat2
            , " return ID(reaction)"]
      where
        reagents' = zip [(0::Int)..] reagents
        products' = zip [(0::Int)..] products
        rs0 = T.pack $ intercalate ", " $ map (\(i, _) -> printf "(r%d)" i) reagents'
        ps0 = T.pack $ intercalate ", " $ map (\(i, _) -> printf "(p%d)" i) products'
        cat0 = maybe "" (\_ -> ", (catalyst)") catalyst
        rs1 = T.pack $ intercalate " and " $ map (\(i, r) -> printf "ID(r%d) = %d" i (moleculeId $ snd r)) reagents'
        ps1 = T.pack $ intercalate " and " $ map (\(i, p) -> printf "ID(p%d) = %d" i (moleculeId $ snd p)) products'
        cat1 = maybe "" (\c -> T.pack $ printf " and ID(catalyst) = %d" (catalystId c)) catalyst
        rs2 = let f (i, (n, _)) = printf "(r%d)-[:REAGENT_IN { amount: %f }]->(reaction)" i n
              in T.pack $ intercalate ", " $ map f reagents'
        ps2 = let f (i, (n, _)) = printf "(reaction)-[:PRODUCT_FROM { amount: %f }]->(p%d)" n i
              in T.pack $ intercalate ", " $ map f products'
        cat2 = maybe "" 
                (\a -> 
                    let format = ", (catalyst)-[:ACCELERATE { temperature: %f, pressure: %f }]->(reaction)"
                        t = accelerateTemperature a
                        p = acceleratePressure a
                    in T.pack $ printf format t p) 
                accelerate
    params = M.fromList [("reactionName", T reactionName)]

{- напишите функцию, которая по номеру реакции в базе будет возвращать её в
   haskell-объект; -}
findReactionById 
    :: Int 
    -> BoltActionT IO
        (Maybe 
            ( Reaction
            , [(Float, Molecule)] {- reagents -}
            , [(Float, Molecule)] {- products -}
            , Maybe Catalyst
            ))
findReactionById id = do 
    result <- queryP cypher params
    if null result
        then pure Nothing
        else 
            let firstRow = head result
                (Just (S structure)) = M.lookup "r" firstRow
                (M props) = fields structure !! 2
                (Just (T name)) = M.lookup "name" props
                reaction = Reaction id name

                rowHasLabel label row = fromMaybe False $ do
                    (S structure) <- M.lookup "c" row
                    let (T l) = fields structure !! 3
                    pure $ l == label

                reagentRows = filter (rowHasLabel "REAGENT_IN") result
                f reagentRow = 
                    let (Just (S structure)) = M.lookup "c" reagentRow
                        (M props) = fields structure !! 4
                        (Just (F n)) = M.lookup "amount" props
                        (Just (S structure')) = M.lookup "a" reagentRow
                        [I id, _, M props'] = fields structure'
                        (Just (T smiles)) = M.lookup "smiles" props'
                        (Just (T iupac)) = M.lookup "iupacName" props'
                        molecule = Molecule id smiles iupac
                    in (realToFrac n, molecule)
                reagents = map f reagentRows

                productRows = filter (rowHasLabel "PRODUCT_FROM") result
                products = map f productRows

                catalystRows = filter (rowHasLabel "ACCELERATE") result
                catalyst = 
                    case catalystRows of
                        [] -> Nothing
                        [row] -> 
                            let (Just (S structure)) = M.lookup "a" row
                                [I id, _, M props] = fields structure
                                (Just (T smiles)) = M.lookup "smiles" props
                                (Just (T name)) = M.lookup "name" props
                            in Just $ Catalyst id smiles name

            in pure $ Just (reaction, reagents, products, catalyst)
  where
    cypher = T.unwords 
                [ "match (r:Reaction),"
                , " (a)-[c:REAGENT_IN|:PRODUCT_FROM|:ACCELERATE]-(r)"
                , " where id(r)={id}"
                , " return r, a, c"
                ]
    params = M.fromList [("id", I id)]

{- напишите функцию, которая по двум заданным молекулам ищет путь через реакции
   и молекулы с наименьшей длинной; -}
findShortestPathBetweenMolecules 
    :: Molecule 
    -> Molecule 
    -> BoltActionT IO 
        [( Either Molecule Reaction
         , Either ReagentIn ProductFrom
         , Either Molecule Reaction)]
findShortestPathBetweenMolecules moleculeA moleculeB = do
    [result] <- queryP cypher params
    let (Just (S struct)) = M.lookup "p" result
    forM_ (fields struct) $ \(L l) -> do
        forM_ l $ \i -> do
            liftIO $ print i
        liftIO $ putStrLn ""
    pure []
    let [L nodes, L edges, _] = fields struct
    let moleculesOrReactions = map moleculeOrReactionFromStruct nodes
    let reagentsOrProducts = map reagentOrProductFromStruct edges
    pure $ zip3 moleculesOrReactions reagentsOrProducts (tail moleculesOrReactions) 
  where
    moleculeOrReactionFromStruct (S struct) 
        | label == "Molecule" = Left $ Molecule id smiles iupac
        | label == "Reaction" = Right $ Reaction id name
      where
        [I id, L [T label], M props] = fields struct
        (Just (T smiles)) = M.lookup "smiles" props 
        (Just (T iupac)) = M.lookup "iupacName" props 
        (Just (T name)) = M.lookup "name" props
    reagentOrProductFromStruct (S struct) 
        | label == "REAGENT_IN" = Left $ ReagentIn id (realToFrac amount)
        | label == "PRODUCT_FROM" = Right $ ProductFrom id (realToFrac amount)
      where
        [I id, T label, M props] = fields struct
        (Just (F amount)) = M.lookup "amount" props
    cypher = 
        T.concat
            [ "match (a), (b),"
            , " p=shortestPath((a)-[*]-(b))"
            , " where id(a)={aId} and id(b)={bId}"
            , " return p"
            ]
    aId = moleculeId moleculeA
    bId = moleculeId moleculeB
    params = M.fromList [("aId", I aId), ("bId", I bId)]
