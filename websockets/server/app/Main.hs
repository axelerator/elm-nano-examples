{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import           Control.Monad      (forever)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import Data.Text (Text)

meow :: WS.Connection -> IO ()
meow conn = forever $ do
    msg <- WS.receiveData conn
    WS.sendTextData conn $ msg `T.append` ", meow"

application :: WS.PendingConnection -> IO ()
application pending = do
    conn <- WS.acceptRequest pending
    meow conn

main :: IO ()
main = do
    WS.runServer "127.0.0.1" 9160 $ application

