module Main where

import System.Environment
import Game
import Models

main :: IO ()
main =  do
  args <- getArgs
  case args of
    ["attack", gameId, "x"] -> startGame $ GameSetup Attack gameId "1" 'x'
    ["attack", gameId, "o"] -> startGame $ GameSetup Attack gameId "1" 'o'
    ["defend", gameId, "x"] -> startGame $ GameSetup Defend gameId "2" 'x'
    ["defend", gameId, "o"] -> startGame $ GameSetup Defend gameId "2" 'o'
    _ -> putStrLn "USAGE: gameMode (attack or defend), gameId, symbol (x or o)"