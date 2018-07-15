module Model where

import Data.Text
import Text.Printf

{- создайте соответствующие типы в haskell-библиотеке -}

data Molecule = 
    Molecule 
    { moleculeId :: Int
    , moleculeSmiles :: Text
    , moleculeIupacName :: Text
    }

data Reaction = 
    Reaction 
    { reactionId :: Int
    , reactionName :: Text
    }

data Catalyst =
    Catalyst
    { catalystId :: Int
    , catalystSmiles :: Text
    , catalystName :: Text
    }

data ReagentIn =
    ReagentIn
    { reagentInId :: Int
    , reagentInAmount :: Float
    }

data ProductFrom =
    ProductFrom
    { productFromId :: Int
    , productFromAmount :: Float
    }

data Accelerate =
    Accelerate
    { accelerateId :: Int
    , accelerateTemperature :: Float
    , acceleratePressure :: Float
    }

instance Show Molecule where
    show (Molecule id _ name) = printf "Molecule { id=%d, name=%s }" id name

instance Show Reaction where
    show (Reaction id name) = printf "Reaction { id=%d, name=%s }" id name

instance Show Catalyst where
    show (Catalyst id _ name) = printf "Catalyst { id=%d, name=%s }" id name

instance Show ReagentIn where
    show (ReagentIn id amount) = printf "ReagentIn { id=%d, amount=%f }" id amount

instance Show ProductFrom where
    show (ProductFrom id amount) = printf "ProductFrom { id=%d, amount=%f }" id amount

instance Show Accelerate where
    show (Accelerate id t p) = printf "Accelerate { id=%d, t=%f, p=%f }" id t p
