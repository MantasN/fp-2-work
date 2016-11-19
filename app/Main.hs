module Main where

import System.Environment
import Game
import Models

main :: IO ()
main =  do
  args <- getArgs
  case args of
    ["1", gameId, symbol] -> startGame $ GameSetup Attack gameId "1" (head symbol)
    ["2", gameId, symbol] -> startGame $ GameSetup Defend gameId "2" (head symbol)
    _ -> putStrLn "USAGE: gameMode (1 - attack, 2 - defend), gameId, symbol"