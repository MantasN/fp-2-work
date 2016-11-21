{-# LANGUAGE DeriveGeneric #-}

module Models
where

import Data.Aeson
import GHC.Generics (Generic)

data GameMode = Attack | Defend
  deriving (Show, Eq)

data GameSetup = GameSetup {
    gameMode :: GameMode
  , gameId :: String
  , playerId :: String
  , symbol :: Char
} deriving Show

data Move = Move {
    x :: Int
  , y :: Int
  , v :: Char
} deriving (Show, Generic)

instance Eq Move where
  a == b = (x a == x b) && (y a == y b) && (v a == v b)

type Moves = [Move]

instance FromJSON Move
instance ToJSON Move