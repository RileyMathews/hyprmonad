{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module HyprLib.Models (Monitor, keywordRestoreCommand) where

import Data.Aeson
import GHC.Generics
import Data.List (intercalate)

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

keywordRestoreCommand :: Monitor -> String
keywordRestoreCommand monitor = do
    let commandPieces = [name monitor, monitorDimensions monitor, monitorPosition monitor, show $ scale monitor]
    "keyword monitor " ++ intercalate "," commandPieces

monitorDimensions :: Monitor -> String
monitorDimensions monitor = (show $ width monitor) ++ "x" ++ (show $ height monitor)

monitorPosition :: Monitor -> String
monitorPosition monitor = (show $ x monitor) ++ "x" ++ (show $ y monitor)
