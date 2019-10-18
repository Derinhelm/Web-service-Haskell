{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DerivingStrategies #-}
module MyType where
  
import Data.Aeson
import Data.Int
import Data.Text(pack, Text)
import Data.Swagger
import GHC.Generics

data Creator = Creator 
  { label :: Text
  } 
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving ToSchema

data Renamer = Renamer 
  { id :: Int64
  , newLabel :: Text
  } 
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving ToSchema

data Linker = Linker 
  { idFrom :: Int64
  , idTo :: Int64
  } 
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving ToSchema


