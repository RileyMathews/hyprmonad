{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module HyprLib.Models (Monitor, keywordRestoreCommand, keywordDisableCommand) where

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

instance Eq Monitor where
    (==) monitor1 monitor2 = HyprLib.Models.id monitor1 == HyprLib.Models.id monitor2

keywordRestoreCommand :: Monitor -> String
keywordRestoreCommand monitor = do
    let commandPieces = [name monitor, monitorDimensions monitor, monitorPosition monitor, show $ scale monitor]
    "keyword monitor " ++ intercalate "," commandPieces

keywordDisableCommand :: Monitor -> String
keywordDisableCommand monitor = "keyword monitor " ++ name monitor ++ ",disable"

monitorDimensions :: Monitor -> String
monitorDimensions monitor = (show $ width monitor) ++ "x" ++ (show $ height monitor)

monitorPosition :: Monitor -> String
monitorPosition monitor = (show $ x monitor) ++ "x" ++ (show $ y monitor)
