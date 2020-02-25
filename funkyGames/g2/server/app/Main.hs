{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.List (find)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, forkIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as DTL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8
import qualified Network.WebSockets as WS
import Control.Concurrent (threadDelay)
import System.Environment

import Data.Aeson
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TLIO

import JsonUtils
import Client as C
import Lib as G

import GameServer as GS
import FizzBuzz as FB

addClient :: Client -> ServerState -> ServerState
addClient client (gs, clients) = (gs, client : clients)

removeClient :: Client -> ServerState -> ServerState
removeClient client (gs, clients) = (gs, (filter ((/= fst client) . fst) clients))

playCardS :: ServerState -> PlayerId -> Int -> ServerState
playCardS (gameState, clients) playerId handIdx =
  let
    updatedGame = G.playCard (currentGame gameState) playerId handIdx
  in
    (gameState { currentGame = updatedGame } , clients)

processClientMsg state (Authorized playerName playerId) (Main.PlayCard idx) = do
  modifyMVar_ state $ \s -> do
      let 
        (gameState, clients) = s
        s' = playCardS s playerId idx
        (newGS, _) = s'
      putStrLn $ playerName ++ " played their " ++ (show idx) ++ " card"
      forM_ clients $ \(clientState, conn) -> do
        --T.putStrLn (toJsonText (sendGame s' clientState) )
        WS.sendTextData conn (toJsonText (sendGame s' clientState) )
      return s'

processClientMsg _ _ _ = do
  putStrLn "NoOp"

broadcast :: Text -> ServerState -> IO ()
broadcast message (gs, clients) = do
    putStrLn (show (parseClientMsg message))
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

parseClientMsg :: Text -> ClientMsg
parseClientMsg txt = 
  case fromJsonText txt of
    Right msg -> msg
    Left err  -> UnknownClientMsg err

users :: [Credentials]
users = [("Axel", "Axel", 0), ("Laurine", "Laurine", 1)]

hasMatchingToken :: String -> Credentials -> Bool
hasMatchingToken token (_, otherToken, _) = token == otherToken

findPlayerByToken :: String -> ClientState
findPlayerByToken token =
  let
    tpl = Data.List.find (hasMatchingToken token) users
  in
    case tpl of
      Just (userId, _, playerId) -> Authorized userId playerId
      Nothing -> Unauthorized

authenticateClient :: Text -> ClientState
authenticateClient msg =
  let
    parsedMsg = parseClientMsg msg
    user = case parsedMsg of
             Auth t -> findPlayerByToken t
             _ -> Unauthorized
  in
    user

sendGame :: ServerState -> ClientState -> ServerMsg
sendGame _ Unauthorized = NothingToSee
sendGame (gameState, _) (Authorized userName playerId) =
  let
    games = gameForPlayers $ currentGame gameState
    (_, game) = games !! playerId
  in
    ShowGame game 

drawCardS :: ServerState -> ServerState
drawCardS (gameState, clients) =
  (gameState { currentGame = G.drawCard (currentGame gameState) 1 }
  , clients)

changeGame state =
  modifyMVar_ state $ \s -> do
      let 
        (gameState, clients) = s
        s' = drawCardS s
        (newGS, _) = s'
      forM_ clients $ \(clientState, conn) -> 
        WS.sendTextData conn (toJsonText (sendGame s' clientState) )
      return s'

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
                  WS.sendTextData conn (toJsonText (sendGame s clientState) )
                  return s'
              putStrLn $ (show clientState) ++ " connected"
              readClientAction client state
          where
            clientState = authenticateClient msg
            client     = (clientState, conn)
            disconnect = do
              -- Remove client and return new state
              s <- modifyMVar state $ \s ->
                let s' = removeClient client s in return (s', s')
              broadcast ("someone disconnected") s

type Client = (ClientState, WS.Connection) 

type Credentials = (String, String, Int)

data ServerMsg 
  = PlayerOnline String
  | ShowGame C.Game
  | NothingToSee

instance ToJSON ServerMsg where
  toJSON (PlayerOnline displayName) = 
    object [ "type" .= ("PlayerOnline"::Text)
           , "name" .= displayName 
           ]
  toJSON (NothingToSee) = 
    object [ "type" .= ("NothingToSee"::Text)
           ]
  toJSON (ShowGame game) = 
    object [ "type" .= ("ShowGame"::Text)
           , "game" .= game 
           ]

data ClientMsg
  = Auth String
  | PlayCard Int
  | None
  | UnknownClientMsg String
  deriving (Show)

instance FromJSON ClientMsg  where
  parseJSON = withObject "Action" $ \o -> do
    msgKind <- o .: "type"
    case msgKind of
      "None" -> return None
      "Auth" -> Auth <$> o .: "token" 
      "PlayCard" -> Main.PlayCard <$> o .: "handIdx" 
      _        -> return $ UnknownClientMsg ("unknown kind: " ++ msgKind)

data ClientState 
  = Unauthorized
  | Authorized String Int
  deriving (Show, Eq)


readClientAction :: Client -> MVar ServerState -> IO ()
readClientAction (clientState, conn) state = forever $ do
    msg <- WS.receiveData conn
    case clientState of
      Authorized userId _ -> do
        processClientMsg state clientState (parseClientMsg msg)
      _ -> 
        readMVar state >>= broadcast
            ((T.pack ("NOPE")) `mappend` ": " `mappend` msg)


type ServerState = (GameState, [Client])

newServerState :: G.Game -> ServerState
newServerState game = ((GameState 0 game), [])

data GameState = GameState
  { lastEventId :: Int
  , currentGame :: G.Game
  }
  deriving (Show)

instance ToJSON GameState where
  toJSON (GameState eId _) = object [ "name" .= (show eId)] 


main :: IO ()
main = do
  args <- getArgs
  GS.runServer args FB.fizzBuzz 

xmain = do
  game <- aGame
  state <- newMVar $ newServerState game
  WS.runServer "127.0.0.1" 9160 $ application state



