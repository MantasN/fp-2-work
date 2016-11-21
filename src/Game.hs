{-# LANGUAGE OverloadedStrings #-}

module Game
    ( startGame
    ) where

import Data.Char
import Data.List
import Control.Monad
import Models
import Network
import Strategy

startGame :: GameSetup -> IO ()
startGame gameSetup = do
  when (mode == Attack) $ void $ sendMoves gId pId [firstMove playerSymbol]
  makeRegularMoves gId pId playerSymbol True
    where
      mode = gameMode gameSetup
      gId = gameId gameSetup
      pId = playerId gameSetup
      playerSymbol = symbol gameSetup

makeRegularMoves :: String -> String -> Char -> Bool -> IO ()
makeRegularMoves gId pId playerSymbol True = do
  moves <- getMoves gId pId
  makeNextMove gId pId playerSymbol moves
makeRegularMoves _ _ _ False = print $ "Game over!"

makeNextMove :: String -> String -> Char -> Maybe Moves -> IO ()
makeNextMove gId pId playerSymbol (Just moves)
  | currentBoardSize < 9 && (not $ winnerExists moves) = do
      moveToTry <- nextMove playerSymbol moves
      case moveToTry of
          Just move -> void $ sendMoves gId pId (moves ++ [move])
          Nothing -> do
              print $ "Do not know where to move!"
              makeRegularMoves gId pId playerSymbol False
      if currentBoardSize /= 8
          then makeRegularMoves gId pId playerSymbol True
          else makeRegularMoves gId pId playerSymbol False
  | otherwise = makeRegularMoves gId pId playerSymbol False
    where
      currentBoardSize = length moves
makeNextMove gId pId playerSymbol Nothing = makeRegularMoves gId pId playerSymbol False