{-# LANGUAGE OverloadedStrings #-}

module Game
    ( startGame
    ) where

import Data.Char
import Data.Aeson
import Data.List
import Network.HTTP.Simple
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Models
import System.Random

gameUrl :: String -> String -> String
gameUrl gId pId = "http://tictactoe.homedir.eu/game/" ++ gId ++ "/player/" ++ pId

firstMove :: Char -> Move
firstMove symbol = Move 1 1 symbol

startGame :: GameSetup -> IO ()
startGame gameSetup = do
  when (mode == Attack) $ void $ sendMoves url [firstMove playerSymbol]
  makeNextMoves url playerSymbol True
    where
      url = gameUrl (gameId gameSetup) (playerId gameSetup)
      playerSymbol = symbol gameSetup
      mode = gameMode gameSetup

makeNextMoves :: String -> Char -> Bool -> IO ()
makeNextMoves _ _ False = print $ "game over"
makeNextMoves url playerSymbol True = do
  moves <- getMoves url
  makeMove url playerSymbol moves

makeMove url playerSymbol (Just moves)
  | boardSize < 9 && (not $ winnerExists moves) = do
      moveToTry <- toGoNext playerSymbol moves
      void $ moveIt url moves moveToTry
      if boardSize /= 8
          then makeNextMoves url playerSymbol True else makeNextMoves url playerSymbol False   
  | otherwise = makeNextMoves url playerSymbol False
    where
    boardSize = length moves
makeMove url playerSymbol Nothing = makeNextMoves url playerSymbol False

moveIt url moves (Just move) = sendMoves url (moves ++ [move])
moveIt url moves Nothing = print $ "do not know where to move"

winnerExists :: Moves -> Bool
winnerExists moves = False

rowMoves row playerSymbol = [move | x <- [row], y <- [0..2], v <- [playerSymbol], let move = (Move x y v)]
columnMoves column playerSymbol = [move | x <- [0..2], y <- [column], v <- [playerSymbol], let move = (Move x y v)]
diagonalMoves 1 playerSymbol = [Move 0 0 playerSymbol, Move 1 1 playerSymbol, Move 2 2 playerSymbol]
diagonalMoves 2 playerSymbol = [Move 0 2 playerSymbol, Move 1 1 playerSymbol, Move 2 0 playerSymbol]

opponent :: Char -> Char
opponent 'x' = 'o'
opponent 'o' = 'x'

inverseMovePlayer :: Move -> Move
inverseMovePlayer (Move x y 'x') = Move x y 'o'
inverseMovePlayer (Move x y 'o') = Move x y 'x'

oneToWin movesGen num playerSymbol moves = 
    if length ourMoves == 2 && length opponentMoves == 0 
        then Just (head (possibleOurMoves \\ ourMoves)) 
        else Nothing
    where
        possibleOurMoves = movesGen num playerSymbol
        ourMoves = possibleOurMoves `intersect` moves
        opponentMoves = (movesGen num (opponent playerSymbol)) `intersect` moves

winInARow :: Char -> Moves -> Maybe Move
winInARow playerSymbol moves = foldl (\result row -> if result /= Nothing then result else oneToWin (rowMoves) row playerSymbol moves) Nothing [1, 2, 3]
winInAColumn :: Char -> Moves -> Maybe Move
winInAColumn playerSymbol moves = foldl (\result column -> if result /= Nothing then result else oneToWin (columnMoves) column playerSymbol moves) Nothing [1, 2, 3]
winInADiagonal :: Char -> Moves -> Maybe Move
winInADiagonal playerSymbol moves = foldl (\result diagonal -> if result /= Nothing then result else oneToWin (diagonalMoves) diagonal playerSymbol moves) Nothing [1, 2]

oneToDefend movesGen num playerSymbol moves = 
    if length ourMoves == 0 && length opponentMoves == 2
        then Just (inverseMovePlayer (head (possibleOpponentMoves \\ opponentMoves)))
        else Nothing
    where
        possibleOurMoves = movesGen num playerSymbol
        ourMoves = possibleOurMoves `intersect` moves
        possibleOpponentMoves = movesGen num (opponent playerSymbol)
        opponentMoves = possibleOpponentMoves `intersect` moves

defendInARow :: Char -> Moves -> Maybe Move
defendInARow playerSymbol moves = foldl (\result row -> if result /= Nothing then result else oneToDefend (rowMoves) row playerSymbol moves) Nothing [1, 2, 3]
defendInAColumn :: Char -> Moves -> Maybe Move
defendInAColumn playerSymbol moves = foldl (\result column -> if result /= Nothing then result else oneToDefend (columnMoves) column playerSymbol moves) Nothing [1, 2, 3]
defendInADiagonal :: Char -> Moves -> Maybe Move
defendInADiagonal playerSymbol moves = foldl (\result diagonal -> if result /= Nothing then result else oneToDefend (diagonalMoves) diagonal playerSymbol moves) Nothing [1, 2]

oneNextToUs movesGen num playerSymbol moves = 
    if length ourMoves == 1 && length opponentMoves == 0
        then Just (head (possibleOurMoves \\ ourMoves))
        else Nothing
    where
        possibleOurMoves = movesGen num playerSymbol
        ourMoves = possibleOurMoves `intersect` moves
        possibleOpponentMoves = movesGen num (opponent playerSymbol)
        opponentMoves = possibleOpponentMoves `intersect` moves

canWinInARow :: Char -> Moves -> Maybe Move
canWinInARow playerSymbol moves = foldl (\result row -> if result /= Nothing then result else oneNextToUs (rowMoves) row playerSymbol moves) Nothing [1, 2, 3]
canWinInAColumn :: Char -> Moves -> Maybe Move
canWinInAColumn playerSymbol moves = foldl (\result column -> if result /= Nothing then result else oneNextToUs (columnMoves) column playerSymbol moves) Nothing [1, 2, 3]
canWinInADiagonal :: Char -> Moves -> Maybe Move
canWinInADiagonal playerSymbol moves = foldl (\result diagonal -> if result /= Nothing then result else oneNextToUs (diagonalMoves) diagonal playerSymbol moves) Nothing [1, 2]

randomInt :: IO Int
randomInt = randomRIO (0, 10)

randomMove :: Char -> Moves -> IO (Maybe Move)
randomMove playerSymbol moves = do
    ind <- randomRIO (0, movesLength - 1)
    return $ Just (possibleMoves !! ind)
    where
        possibleMoves = [move | x <- [0..2], y <- [0..2], v <- [playerSymbol], let move = (Move x y v), move `notElem` moves]
        movesLength = length moves

toGoNext :: Char -> Moves -> IO (Maybe Move)
toGoNext playerSymbol moves = do
    randMov <- randomMove playerSymbol moves
    return $ msum [winInARow playerSymbol moves, winInAColumn playerSymbol moves, winInADiagonal playerSymbol moves, defendInARow playerSymbol moves, defendInAColumn playerSymbol moves, defendInADiagonal playerSymbol moves, canWinInARow playerSymbol moves, canWinInAColumn playerSymbol moves, canWinInADiagonal playerSymbol moves, randMov]

getMoves :: String -> IO (Maybe Moves)
getMoves givenUrl = do
    request' <- parseRequest givenUrl
    let request = setRequestHeader "Accept" ["application/json"] 
                  $ request'
    response <- httpLBS request
    return $ (decode (getResponseBody response) :: Maybe Moves)

sendMoves :: String -> Moves -> IO ()
sendMoves givenUrl moves = do
    request' <- parseRequest postUrl
    let request = setRequestBodyLBS encodedMoves
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ request'
    response <- httpLBS request
    void $ print $ getResponseStatusCode response
    where
      encodedMoves = encode moves
      postUrl = "POST " ++ givenUrl