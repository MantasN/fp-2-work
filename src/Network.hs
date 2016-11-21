{-# LANGUAGE OverloadedStrings #-}

module Network
where

import Models
import Network.HTTP.Simple
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Control.Monad

gameUrl :: String -> String -> String
gameUrl gId pId = "http://tictactoe.homedir.eu/game/" ++ gId ++ "/player/" ++ pId

getMoves :: String -> String -> IO (Maybe Moves)
getMoves gId pId = do
    request' <- parseRequest getUrl
    let request = setRequestHeader "Accept" ["application/json"] 
                  $ request'
    response <- httpLBS request
    void $ print $ getResponseStatusCode response
    return $ (decode (getResponseBody response) :: Maybe Moves)
    where
      getUrl = gameUrl gId pId

sendMoves :: String -> String -> Moves -> IO ()
sendMoves gId pId moves = do
    request' <- parseRequest postUrl
    let request = setRequestBodyLBS encodedMoves
                  $ setRequestHeader "Content-Type" ["application/json"]
                  $ request'
    response <- httpLBS request
    void $ print $ getResponseStatusCode response
    where
      encodedMoves = encode moves
      postUrl = "POST " ++ (gameUrl gId pId)