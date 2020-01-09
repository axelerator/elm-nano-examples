{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as DTL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL

import qualified Network.WebSockets as WS

import Data.Aeson
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TLIO

addClient :: Client -> ServerState -> ServerState
addClient client (gs, clients) = (gs, client : clients)

removeClient :: Client -> ServerState -> ServerState
removeClient client (gs, clients) = (gs, (filter ((/= fst client) . fst) clients))

broadcast :: Text -> ServerState -> IO ()
broadcast message (_, clients) = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
     msg <- WS.receiveData conn :: IO Text
     (_, clients) <- readMVar state
     case msg of
       _ -> flip finally disconnect $ do
              modifyMVar_ state $ \s -> do
                  let s' = addClient client s
                  --WS.sendTextData conn ( "Welcome! Users: " :: Text)
                  WS.sendTextData conn (byteStringToText $ encode (forClient gameT1 0))
                  return s'
          where
            client     = (msg, conn)
            disconnect = do
              -- Remove client and return new state
              s <- modifyMVar state $ \s ->
                let s' = removeClient client s in return (s', s')
              broadcast (fst client <> " disconnected") s

type Client = (Text, WS.Connection) 

type ServerState = ([Game], [Client])

newServerState :: ServerState
newServerState = ([initGame], [])

main :: IO ()
main = do
    TLIO.putStrLn . TLE.decodeUtf8 . encode $ (forClient gameT1 0)
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state

byteStringToText :: BSL.ByteString -> Text
byteStringToText  = T.pack . C8.unpack . BSL.toStrict


