{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module HyprLib.Models where

import Data.Aeson
import GHC.Generics

data Monitor = Monitor
    { id :: Int
    , name :: String
    , x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    , scale :: Float
    }
    deriving (Show, Generic, ToJSON, FromJSON)
