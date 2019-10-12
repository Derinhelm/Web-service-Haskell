{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module MyType where

import Data.Int
import Data.Aeson
import GHC.Generics

data Creator = Creator 
    {   label :: String
    } deriving (Eq, Show, Generic, FromJSON)
instance ToJSON Creator


data Renamer = Renamer 
    {   id :: Int64,
        newLabel :: Char
    } deriving (Eq, Show, Generic, FromJSON)
instance ToJSON Renamer

data Linker = Linker 
    {   idFrom :: Int64,
        idTo :: Int64
    } deriving (Eq, Show, Generic, FromJSON)
instance ToJSON Linker


