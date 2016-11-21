module Strategy
where

import Models
import Data.Char
import Data.List
import Control.Monad
import System.Random

opponent :: Char -> Char
opponent 'x' = 'o'
opponent 'o' = 'x'

inverseMovePlayer :: Move -> Move
inverseMovePlayer (Move x y 'x') = Move x y 'o'
inverseMovePlayer (Move x y 'o') = Move x y 'x'

winnerExists :: Moves -> Bool
winnerExists moves = foldl (||) False (inARow ++ inAColumn ++ inADiagonal)
    where
        inARow = map (threeInALine moves allRowMoves) [1, 2, 3]
        inAColumn = map (threeInALine moves allColumnMoves) [1, 2, 3]
        inADiagonal = map (threeInALine moves allDiagonalMoves) [1, 2]
        threeInALine currentMoves movesGen i = length xMoves == 3 || length oMoves == 3
            where
                xMoves = movesGen i 'x' `intersect` moves
                oMoves = movesGen i 'o' `intersect` moves

firstMove :: Char -> Move
firstMove symbol = Move 1 1 symbol

allRowMoves :: Int -> Char -> Moves
allRowMoves rowN playerSymbol = [move | x <- [rowN], y <- [0..2], v <- [playerSymbol], let move = (Move x y v)]

allColumnMoves :: Int -> Char -> Moves
allColumnMoves columnN playerSymbol = [move | x <- [0..2], y <- [columnN], v <- [playerSymbol], let move = (Move x y v)]

allDiagonalMoves :: Int -> Char -> Moves
allDiagonalMoves 1 playerSymbol = [Move 0 0 playerSymbol, Move 1 1 playerSymbol, Move 2 2 playerSymbol]
allDiagonalMoves 2 playerSymbol = [Move 0 2 playerSymbol, Move 1 1 playerSymbol, Move 2 0 playerSymbol]

allCornerMoves :: Char -> Moves
allCornerMoves playerSymbol = [move | x <- [0,2], y <- [0,2], v <- [playerSymbol], let move = (Move x y v)]

checkRow :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> [Maybe Move]
checkRow criteria playerSymbol moves = map (checkByCriteria (criteria) playerSymbol moves (allRowMoves)) [1, 2, 3]

checkColumn :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> [Maybe Move]
checkColumn criteria playerSymbol moves = map (checkByCriteria (criteria) playerSymbol moves (allColumnMoves)) [1, 2, 3]

checkDiagonal :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> [Maybe Move]
checkDiagonal criteria playerSymbol moves = map (checkByCriteria (criteria) playerSymbol moves (allDiagonalMoves)) [1, 2]

checkByCriteria :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> (Int -> Char -> Moves) -> Int -> Maybe Move
checkByCriteria criteria playerSymbol moves allPossibleMovesGen index = criteria possibleOurMoves possibleOpponentMoves moves
    where
        possibleOurMoves = allPossibleMovesGen index playerSymbol
        possibleOpponentMoves = allPossibleMovesGen index (opponent playerSymbol)

matchCriteria :: (Moves -> Moves -> Moves -> Maybe Move) -> Char -> Moves -> Maybe Move
matchCriteria criteria playerSymbol moves = msum (inARow ++ inAColumn ++ inADiagonal)
    where
      inARow = checkRow criteria playerSymbol moves
      inAColumn = checkColumn criteria playerSymbol moves
      inADiagonal = checkDiagonal criteria playerSymbol moves

oneToDefend :: Moves -> Moves -> Moves -> Maybe Move
oneToDefend possibleOurMoves possibleOpponentMoves moves = 
    if length ourMoves == 0 && length opponentMoves == 2
        then Just (inverseMovePlayer (head (possibleOpponentMoves \\ opponentMoves)))
        else Nothing
    where
        ourMoves = possibleOurMoves `intersect` moves
        opponentMoves = possibleOpponentMoves `intersect` moves
        
nToWin :: Int -> Moves -> Moves -> Moves -> Maybe Move
nToWin left possibleOurMoves possibleOpponentMoves moves = 
    if length ourMoves == (3 - left) && length opponentMoves == 0 
        then Just (head (possibleOurMoves \\ ourMoves)) 
        else Nothing
    where
        ourMoves = possibleOurMoves `intersect` moves
        opponentMoves = possibleOpponentMoves `intersect` moves

randomMove :: Char -> Moves -> IO (Maybe Move)
randomMove playerSymbol moves = if movesLength > 0
    then do
        index <- randomRIO (0, movesLength - 1)
        return $ Just (movesToSelectFrom !! index)
    else return Nothing
    where
        possibleOurMoves = [move | x <- [0..2], y <- [0..2], v <- [playerSymbol], let move = (Move x y v), move `notElem` moves]
        availableMoves = filter (\move -> (inverseMovePlayer move) `notElem` moves) possibleOurMoves
        cornerMoves = availableMoves `intersect` (allCornerMoves playerSymbol)
        movesToSelectFrom = if length cornerMoves > 0 then cornerMoves else availableMoves
        movesLength = length movesToSelectFrom

nextMove :: Char -> Moves -> IO (Maybe Move)
nextMove playerSymbol moves = do
    random <- randomMove playerSymbol moves
    return $ msum [matchCriteria (nToWin 1) playerSymbol moves, matchCriteria oneToDefend playerSymbol moves, matchCriteria (nToWin 2) playerSymbol moves, random]