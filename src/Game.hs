{-# LANGUAGE OverloadedStrings #-}

module Game
    ( startGame
    ) where

import Data.Char
import Data.Aeson
import Network.HTTP.Simple
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Models

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

makeNextMoves _ _ False = print $ "game over"
makeNextMoves url playerSymbol True = do
  moves <- getMoves url
  makeMove url playerSymbol moves

makeMove url playerSymbol (Just moves)
  | boardSize < 9 && (not $ winnerExists moves) = do
      void $ sendMoves url (moves ++ [nextMove playerSymbol moves])
      if boardSize /= 8
          then makeNextMoves url playerSymbol True else makeNextMoves url playerSymbol False   
  | otherwise = makeNextMoves url playerSymbol False
    where
    boardSize = length moves
makeMove url playerSymbol Nothing = makeNextMoves url playerSymbol False

nextMove :: Char -> Moves -> Move
nextMove playerSymbol moves = head [move | x <- [0..2], y <- [0..2], v <- [playerSymbol], let move = (Move x y v), move `notElem` moves]

winnerExists :: Moves -> Bool
winnerExists moves = if (length moves) == 5 then True else False

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