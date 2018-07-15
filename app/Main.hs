{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Control.Monad
import Data.Default
import Database.Bolt
import Model
import DatabaseOperations
import System.Environment
import Data.Maybe

fillDatabase :: T.Text
fillDatabase = T.unlines
    [ "create"

    {- Molecules -}
    , " (h2o2       :Molecule { smiles: 'OO',                   iupacName: 'Hydrogen Pyroxide' }),"
    , " (h2o        :Molecule { smiles: 'O',                    iupacName: 'Water' }),"
    , " (o2         :Molecule { smiles: 'O=O',                  iupacName: 'Oxygen'}),"
    , " (c2h5oh     :Molecule { smiles: 'CCO',                  iupacName: 'Ethanol'}),"
    , " (c2h4       :Molecule { smiles: 'C=C',                  iupacName: 'Ethene'}),"
    , " (c2h4o      :Molecule { smiles: 'CC=O',                 iupacName: 'Ethanal'}),"
    , " (h2         :Molecule { smiles: '[HH]',                 iupacName: 'Hydrogen'}),"
    , " (c4h6       :Molecule { smiles: 'C(#CC)C',              iupacName: '2-Butyne'}),"
    , " (c4h8       :Molecule { smiles: 'CC=CC',                iupacName: 'Cis-2-Butene'}),"
    , " (c4h10      :Molecule { smiles: 'CCCC',                 iupacName: 'Butane'}),"
    , " (n2         :Molecule { smiles: 'N#N',                  iupacName: 'Nitrogen'}),"
    , " (nh3        :Molecule { smiles: 'N',                    iupacName: 'Ammonia'}),"
    , " (so2        :Molecule { smiles: 'O=S=O',                iupacName: 'Sulfur Dioxide'}),"
    , " (so3        :Molecule { smiles: 'O=S(=O)=O',            iupacName: 'Sulfur Trioxide'}),"
    , " (hi         :Molecule { smiles: 'I',                    iupacName: 'Hidrogen Iodide'}),"
    , " (i2         :Molecule { smiles: 'II',                   iupacName: 'Iodine'}),"
    , " (ch4        :Molecule { smiles: 'C',                    iupacName: 'Methane'}),"
    , " (cl2        :Molecule { smiles: 'ClCl',                 iupacName: 'Chlorine'}),"
    , " (ch3cl      :Molecule { smiles: 'CCl',                  iupacName: 'Methyl Chloride'}),"
    , " (hcl        :Molecule { smiles: 'Cl',                   iupacName: 'Hydrochloric Acid'}),"
    , " (br2        :Molecule { smiles: 'BrBr',                 iupacName: 'Bromine'}),"
    , " (c2h4br2    :Molecule { smiles: 'C(CBr)Br',             iupacName: '1,2-Dibromoethane'}),"
    , " (c2h6       :Molecule { smiles: 'CC',                   iupacName: 'Ethane'}),"
    , " (co2        :Molecule { smiles: 'C(=O)=O',              iupacName: 'Carbon Dioxide'}),"
    , " (hno3       :Molecule { smiles: '[N+](=O)(O)[O-]',      iupacName: 'Nitric Acid'}),"
    , " (ch3no2     :Molecule { smiles: 'C[N+](=O)[O-]',        iupacName: 'Nitromethane'}),"
    , " (c2h2       :Molecule { smiles: 'C#C',                  iupacName: 'Acetylene'}),"
    , " (co         :Molecule { smiles: '[C-]#[O+]',            iupacName: 'Carbon Monoxyde'}),"
    , " (ch3oh      :Molecule { smiles: 'CO',                   iupacName: 'Methyl Alcohol'}),"
    , " (kh         :Molecule { smiles: '[H-].[K+]',            iupacName: 'Potassium Hydride'}),"
    , " (ch3ok      :Molecule { smiles: 'C[O-].[K+]',           iupacName: 'Potassium Methoxide'}),"
    , " (c2h5ono2   :Molecule { smiles: 'C(CO)[N+](=O)[O-]',    iupacName: '2-Nitroethanol'}),"

    {- Catalysts -}
    , " (mno2   :Catalyst { smiles: 'O=[Mn]=O',                 name: 'Dioxomanganese' }),"
    , " (al2o3  :Catalyst { smiles: '[O-2].[O-2].[O-2].[Al+3].[Al+3]', name: 'Aluminum Oxide' }),"
    , " (h2so4  :Catalyst { smiles: 'OS(=O)(=O)O',              name: 'Sulfuric Acid' }),"
    , " (cu     :Catalyst { smiles: '[Cu]',                     name: 'Copper' }),"
    , " (pd     :Catalyst { smiles: '[Pd]',                     name: 'Palladium' }),"
    , " (fe     :Catalyst { smiles: '[Fe]',                     name: 'Iron' }),"
    , " (v2o5   :Catalyst { smiles: 'O=[V](=O)O[V](=O)=O',      name: 'Vanadium Pentoxide' }),"
    , " (au     :Catalyst { smiles: '[Au]',                     name: 'Gold' }),"
    , " (ni     :Catalyst { smiles: '[Ni]',                     name: 'Nikel' }),"
    , " (condition :Catalyst {                                  name: 'Environmental conditions' }),"

    {- Reactions -}
    , " (h2o2Decomp :Reaction { name: 'Decomposition of Hydrogen Pyroxide' }),"
    , " (h2o2)-[:REAGENT_IN { amount: 2.0 }]->(h2o2Decomp),"
    , " (h2o2Decomp)-[:PRODUCT_FROM { amount: 2.0 }]->(h2o),"
    , " (h2o2Decomp)-[:PRODUCT_FROM { amount: 1.0 }]->(o2),"
    , " (mno2)-[:ACCELERATE]->(h2o2Decomp),"

    , " (c2h5ohDecomp :Reaction { name: 'Decomposition of Ethanol' }),"
    , " (c2h5oh)-[:REAGENT_IN { amount: 1.0 }]->(c2h5ohDecomp),"
    , " (c2h5ohDecomp)-[:PRODUCT_FROM { amount: 1.0 }]->(c2h4),"
    , " (c2h5ohDecomp)-[:PRODUCT_FROM { amount: 1.0 }]->(h2o),"
    , " (al2o3)-[:ACCELERATE { temperature: 300.0 }]->(c2h5ohDecomp),"

    , " (c2h5ohDecomp1 :Reaction { name: 'Decomposition of Ethanol' }),"
    , " (c2h5oh)-[:REAGENT_IN { amount: 1.0 }]->(c2h5ohDecomp1),"
    , " (c2h5ohDecomp1)-[:PRODUCT_FROM { amount: 1.0 }]->(c2h4),"
    , " (c2h5ohDecomp1)-[:PRODUCT_FROM { amount: 1.0 }]->(h2o),"
    , " (h2so4)-[:ACCELERATE { temperature: 170.0 }]->(c2h5ohDecomp1),"

    , " (c2h5ohDecomp2 :Reaction { name: 'Decomposition of Ethanol' }),"
    , " (c2h5oh)-[:REAGENT_IN { amount: 1.0 }]->(c2h5ohDecomp2),"
    , " (c2h5ohDecomp2)-[:PRODUCT_FROM { amount: 1.0 }]->(c2h4o),"
    , " (c2h5ohDecomp2)-[:PRODUCT_FROM { amount: 1.0 }]->(h2),"
    , " (cu)-[:ACCELERATE { temperature: 250.0 }]->(c2h5ohDecomp2),"

    , " (c4h6Hydration :Reaction { name: 'Hydration of 2-Butyne' }),"
    , " (c4h6)-[:REAGENT_IN { amount: 1.0 }]->(c4h6Hydration),"
    , " (h2)-[:REAGENT_IN { amount: 1.0 }]->(c4h6Hydration),"
    , " (c4h6Hydration)-[:PRODUCT_FROM { amount: 1.0 }]->(c4h8),"
    , " (pd)-[:ACCELERATE { pressure: 20.0 }]->(c4h6Hydration),"

    , " (c4h6Hydration1 :Reaction { name: 'Hydration of 2-Butyne' }),"
    , " (c4h6)-[:REAGENT_IN { amount: 1.0 }]->(c4h6Hydration1),"
    , " (h2)-[:REAGENT_IN { amount: 2.0 }]->(c4h6Hydration1),"
    , " (c4h6Hydration1)-[:PRODUCT_FROM { amount: 1.0 }]->(c4h10),"
    , " (pd)-[:ACCELERATE { pressure: 20.0 }]->(c4h6Hydration1),"

    , " (ammoniaObtaining :Reaction { name: 'Obtaining of Ammonia' }),"
    , " (n2)-[:REAGENT_IN { amount: 1.0 }]->(ammoniaObtaining),"
    , " (h2)-[:REAGENT_IN { amount: 3.0 }]->(ammoniaObtaining),"
    , " (ammoniaObtaining)-[:PRODUCT_FROM { amount: 2.0 }]->(nh3),"
    , " (fe)-[:ACCELERATE { temperature: 450.0, pressure: 300.0 }]->(ammoniaObtaining),"

    , " (so3Obtaining :Reaction { name: 'Obtaining of Sulfur Trioxide' }),"
    , " (so2)-[:REAGENT_IN { amount: 2.0 }]->(so3Obtaining),"
    , " (o2)-[:REAGENT_IN { amount: 1.0 }]->(so3Obtaining),"
    , " (so3Obtaining)-[:PRODUCT_FROM { amount: 2.0 }]->(so3),"
    , " (v2o5)-[:ACCELERATE { temperature: 450.0 }]->(so3Obtaining),"

    , " (hiDecomp :Reaction { name: 'Hidrogen Iodide decomposition' }),"
    , " (hi)-[:REAGENT_IN { amount: 2.0 }]->(hiDecomp),"
    , " (hiDecomp)-[:PRODUCT_FROM { amount: 1.0 }]->(h2),"
    , " (hiDecomp)-[:PRODUCT_FROM { amount: 1.0 }]->(i2),"
    , " (au)-[:ACCELERATE]->(hiDecomp),"

    , " (ch4Halo :Reaction { name: 'Halogination of Methane' }),"
    , " (ch4)-[:REAGENT_IN { amount: 1.0 }]->(ch4Halo),"
    , " (cl2)-[:REAGENT_IN { amount: 1.0 }]->(ch4Halo),"
    , " (ch4Halo)-[:PRODUCT_FROM { amount: 1.0 }]->(ch3cl),"
    , " (ch4Halo)-[:PRODUCT_FROM { amount: 1.0 }]->(hcl),"

    , " (c2h4Halo :Reaction { name: 'Halogination of Ethene' }),"
    , " (c2h4)-[:REAGENT_IN { amount: 1.0 }]->(c2h4Halo),"
    , " (br2)-[:REAGENT_IN { amount: 1.0 }]->(c2h4Halo),"
    , " (c2h4Halo)-[:PRODUCT_FROM { amount: 1.0 }]->(c2h4br2),"

    , " (c2h6Oxid :Reaction { name: 'Oxidation of Ethane' }),"
    , " (c2h6)-[:REAGENT_IN { amount: 2.0 }]->(c2h6Oxid),"
    , " (o2)-[:REAGENT_IN { amount: 7.0 }]->(c2h6Oxid),"
    , " (c2h6Oxid)-[:PRODUCT_FROM { amount: 4.0 }]->(co2),"
    , " (c2h6Oxid)-[:PRODUCT_FROM { amount: 6.0 }]->(h2o),"

    , " (ch4Halo1 :Reaction { name: 'Halogination of Methane' }),"
    , " (ch4)-[:REAGENT_IN { amount: 1.0 }]->(ch4Halo1),"
    , " (cl2)-[:REAGENT_IN { amount: 1.0 }]->(ch4Halo1),"
    , " (ch4Halo1)-[:PRODUCT_FROM { amount: 1.0 }]->(ch3cl),"
    , " (ch4Halo1)-[:PRODUCT_FROM { amount: 1.0 }]->(h2o),"

    , " (ch4Nitr :Reaction { name: 'Nitration of Methane' }),"
    , " (ch4)-[:REAGENT_IN { amount: 1.0 }]->(ch4Nitr),"
    , " (hno3)-[:REAGENT_IN { amount: 1.0 }]->(ch4Nitr),"
    , " (ch4Nitr)-[:PRODUCT_FROM { amount: 1.0 }]->(ch3no2),"
    , " (ch4Nitr)-[:PRODUCT_FROM { amount: 1.0 }]->(h2o),"

    , " (ch4Pyro :Reaction { name: 'Pyrolysis of Methane' }),"
    , " (ch4)-[:REAGENT_IN { amount: 2.0 }]->(ch4Pyro),"
    , " (ch4Pyro)-[:PRODUCT_FROM { amount: 1.0 }]->(c2h2),"
    , " (ch4Pyro)-[:PRODUCT_FROM { amount: 3.0 }]->(h2),"
    , " (condition)-[:ACCELERATE { temperature: 1500.0 }]->(ch4Pyro),"

    , " (ch4Conv :Reaction { name: 'Conversion of Methane' }),"
    , " (ch4)-[:REAGENT_IN { amount: 1.0 }]->(ch4Conv),"
    , " (h2o)-[:REAGENT_IN { amount: 1.0 }]->(ch4Conv),"
    , " (ch4Conv)-[:PRODUCT_FROM { amount: 1.0 }]->(co),"
    , " (ch4Conv)-[:PRODUCT_FROM { amount: 3.0 }]->(h2),"
    , " (ni)-[:ACCELERATE { temperature: 800.0 }]->(ch4Conv),"

    , " (ch3ohHalo :Reaction { name: 'Halogination of Methyl Alcohol' }),"
    , " (ch3oh)-[:REAGENT_IN { amount: 1.0 }]->(ch3ohHalo),"
    , " (kh)-[:REAGENT_IN { amount: 1.0 }]->(ch3ohHalo),"
    , " (ch3ohHalo)-[:PRODUCT_FROM { amount: 1.0 }]->(ch3ok),"
    , " (ch3ohHalo)-[:PRODUCT_FROM { amount: 1.0 }]->(h2),"

    , " (c2h5ohEther :Reaction { name: 'Etherification of Ethanol' }),"
    , " (c2h5oh)-[:REAGENT_IN { amount: 1.0 }]->(c2h5ohEther),"
    , " (hno3)-[:REAGENT_IN { amount: 1.0 }]->(c2h5ohEther),"
    , " (c2h5ohEther)-[:PRODUCT_FROM { amount: 1.0 }]->(c2h5ono2),"
    , " (c2h5ohEther)-[:PRODUCT_FROM { amount: 1.0 }]->(h2o)"

    ]

createConfiguration :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> BoltCfg
createConfiguration user password host = 
    def 
        { user = fromMaybe "neo4j" user
        , password = fromMaybe "neo4j" password
        , host = T.unpack $ fromMaybe "127.0.0.1" host 
        }

data Request 
    = FillDatabase
    | CreateMolecule T.Text T.Text
    | CreateCatalyst T.Text T.Text 
    | CreateReaction T.Text [(Float, Int)] [(Float, Int)] (Maybe (Int, Float, Float))
    | FindReactionById Int
    | FindShortestPathBetweenMolecules Int Int

data AppConfig 
    = AppConfig BoltCfg Request
    | ShowHelp

parseRequest :: [String] -> Maybe Request
parseRequest ("FillDatabase":rest)
    = Just $ FillDatabase
parseRequest ("CreateMolecule":smiles:iupac:rest)
    = Just $ CreateMolecule (T.pack smiles) (T.pack iupac)
parseRequest ("CreateCatalyst":smiles:name:rest)
    = Just $ CreateCatalyst (T.pack smiles) (T.pack name)
parseRequest ("CreateReaction":reactionName:rest)
    = let (reagents, products, catalyst) = collect ([], [], Nothing) rest
          collect (rs, ps, c) ("--reagent":amount:id:rest) = collect ((read amount, read id):rs, ps, c) rest
          collect (rs, ps, c) ("--product":amount:id:rest) = collect (rs, (read amount, read id):ps, c) rest
          collect (rs, ps, c) ("--catalyst":id:t:p:rest) = collect (rs, ps, Just (read id, read t, read p)) rest
          collect (rs, ps, c) rest = (rs, ps, c)
      in Just $ CreateReaction (T.pack reactionName) reagents products catalyst
parseRequest ("FindReactionById":id:rest)
    = Just $ FindReactionById (read id)
parseRequest ("FindShortestPathBetweenMolecules":id1:id2:rest) 
    = Just $ FindShortestPathBetweenMolecules (read id1) (read id2)
parseRequest _ = Nothing

parseArgs :: IO AppConfig
parseArgs = do
    res <- go (Nothing, Nothing, Nothing) <$> getArgs
    case res of
        Just (user, password, host, rest) ->
            let boltConfig = createConfiguration user password host
            in case parseRequest rest of
                Just request -> pure $ AppConfig boltConfig request
                Nothing -> do
                    putStrLn "Error: can't parse request"
                    pure ShowHelp
        Nothing -> pure ShowHelp
  where
    go (u, p, h) ("--user":user:rest) = go (Just user, p, h) rest
    go (u, p, h) ("--password":password:rest) = go (u, Just password, h) rest
    go (u, p, h) ("--host":host:rest) = go (u, p, Just host) rest
    go (u, p, h) ("--help":rest) = Nothing
    go (u, p, h) rest = Just (T.pack <$> u, T.pack <$> p, T.pack <$> h, rest)

printHelp :: IO ()
printHelp = do
    putStrLn "<app> --help (1)"
    putStrLn "<app> [--user username] [--password password] [--host host]"
    putStrLn "      [ FillDatabase"
    putStrLn "      | CreateMolecule smiles iupac"
    putStrLn "      | CreateCatalyst smiles name"
    putStrLn "      | CreateReaction reactionName"
    putStrLn "          [--reagent amount id]*"
    putStrLn "          [--product amount id]*"
    putStrLn "          [--catalyst id t p]"
    putStrLn "      | FindReactionById smiles iupac"
    putStrLn "      | FindShortestPathBetweenMolecules smiles iupac] (2)"
    putStrLn "  (1) show this message"
    putStrLn "  (2) perform request"
    putStrLn "  database parameters:"
    putStrLn "      user        [default = \"neo4j\"]"
    putStrLn "      password    [default = \"neo4j\"]"
    putStrLn "      host        [default = \"localhost\"]"

performRequest :: Pipe -> Request -> IO ()
performRequest pipe FillDatabase = do
    run pipe $ query fillDatabase
    print "Reactions were added to database"
performRequest pipe (CreateMolecule smiles iupac) 
    = run pipe (createMolecule smiles iupac) >>= print
performRequest pipe (CreateCatalyst smiles name)
    = run pipe (createCatalyst smiles name) >>= print
performRequest pipe (CreateReaction reactionName reagents products catalyst)
    = let f (amount, id) = (amount, Molecule id "" "")
          reagents' = map f reagents 
          products' = map f products
          catalyst' = (\(id, t, p) -> (Catalyst id "" "", Accelerate (-1) t p)) <$> catalyst
      in run pipe (createReaction reactionName reagents' products' catalyst') >>= print
performRequest pipe (FindReactionById id)
    = run pipe (findReactionById id) >>= print
performRequest pipe (FindShortestPathBetweenMolecules idA idB)
    = let moleculeA = Molecule idA "" ""
          moleculeB = Molecule idB "" "" 
      in run pipe (findShortestPathBetweenMolecules moleculeA moleculeB) >>= print

main :: IO ()
main = do 
    config <- parseArgs
    case config of
        AppConfig boltConfig request -> do
            pipe <- connect boltConfig
            performRequest pipe request
            close pipe
        ShowHelp -> printHelp
