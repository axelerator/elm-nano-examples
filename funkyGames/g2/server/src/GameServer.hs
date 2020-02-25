{-# LANGUAGE OverloadedStrings #-}
module GameServer 
  ( runServer
  , GameServerGame (..)
  ) where


import           Control.Concurrent         (MVar, forkIO, modifyMVar,
                                             modifyMVar_, newMVar, readMVar)
import           Control.Concurrent         (threadDelay)
import           Control.Exception          (finally)
import           Control.Monad              (forM_, forever)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8
import           Data.Char                  (isPunctuation, isSpace)
import           Data.List                  (find, intercalate)
import           Data.Monoid                (mappend)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy             as DTL
import qualified Network.WebSockets         as WS

import           Data.Aeson
import qualified Data.Text.Lazy.Encoding    as TLE
import qualified Data.Text.Lazy.IO          as TLIO
import System.Log.Handler (setFormatter)
import System.Log.Logger (setLevel, updateGlobalLogger)  
import System.Log (Priority(..))  


import           Client                     as C
import           JsonUtils
import           Lib                        as G
import           Utils                      (rootLogger, infoM, errorM, textToString)

type ServerState g = (g, [Client])

type Client = (ClientState, WS.Connection)

data ClientState
  = Unauthorized
  | Authorized String Int
  deriving (Show, Eq)

data ClientMsg
  = Auth String
  | InGame
  | None
  | UnknownClientMsg String
  deriving (Show)

instance FromJSON ClientMsg  where
  parseJSON = withObject "Action" $ \o -> do
    msgKind <- o .: "type"
    case msgKind of
      "None"   -> return None
      "Auth"   -> Auth <$> o .: "token"
      "InGame" -> return InGame
      _        -> return $ UnknownClientMsg ("unknown kind: " ++ msgKind)

addClient :: Client -> (ServerState g) -> (ServerState g)
addClient client (gs, clients) = (gs, client : clients)

removeClient :: Client -> (ServerState g) -> (ServerState g)
removeClient client (gs, clients) = (gs, (filter ((/= fst client) . fst) clients))

processClientMsg :: Show a => GameServerGame g a -> MVar (ServerState g) -> ClientState -> Text -> IO ()
processClientMsg gsg state clientState msg = 
  let
    mayBeAction = (parseAction gsg) msg
  in
    case mayBeAction of
      Left err ->
        errorM $ "Unable to parse action :" ++ err
      Right action ->
        do
          modifyMVar_ state $ \s -> do
              let 
                (gameState, clients) = s
                s' = (update gsg) gameState action
              forM_ clients $ \(clientState, conn) -> do
                case clientState of
                  Unauthorized -> return ()
                  Authorized playerName playerIdx -> do
                    infoM $ (clientForLog clientState) ++ " plays: " ++ (show action)
                    WS.sendTextData conn ((serializeGame gsg) s' playerIdx)
              return (s', clients)


readClientAction :: Show a => GameServerGame g a -> Client -> MVar (ServerState g) -> IO ()
readClientAction gsg (clientState, conn) state = forever $ do
    msg <- WS.receiveData conn
    case clientState of
      Authorized userId _ -> do
        processClientMsg gsg state clientState msg
      _ ->
        infoM "Ignoring message from unauthorized client"

parseClientMsg :: Text -> ClientMsg
parseClientMsg txt =
  case fromJsonText txt of
    Right msg -> msg
    Left err  -> UnknownClientMsg err

broadcast :: Text -> ServerState g -> IO ()
broadcast _ _ = return ()

authenticateClient :: Text -> ClientState
authenticateClient msg =
  let
    parsedMsg = parseClientMsg msg
    user = case parsedMsg of
             Auth t -> findUserByToken t
             _      -> Unauthorized
  in
    user

type Credentials = (String, String, Int)

users :: [Credentials]
users = [("Axel", "Axel", 0), ("Laurine", "Laurine", 1)]

hasMatchingToken :: String -> Credentials -> Bool
hasMatchingToken token (_, otherToken, _) = token == otherToken

findUserByToken :: String -> ClientState
findUserByToken token =
  let
    tpl = Data.List.find (hasMatchingToken token) users
  in
    case tpl of
      Just (userId, _, playerId) -> Authorized userId playerId
      Nothing                    -> Unauthorized

application :: Show a => GameServerGame g a -> MVar (ServerState g) -> WS.PendingConnection -> IO ()
application gsg state pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
     msg <- WS.receiveData conn :: IO Text
     (_, clients) <- readMVar state
     case msg of
       _ -> flip finally disconnect $ do
              case clientState of
                Authorized _ playerIdx -> do
                  modifyMVar_ state $ \s -> do
                      let 
                        (gameState, _) = s
                        s' = addClient client s
                      WS.sendTextData conn ((serializeGame gsg) gameState playerIdx)
                      return s'
                  infoM $ (clientForLog clientState) ++ " connected"
                  readClientAction gsg client state
                _              -> return ()  
          where
            clientState = authenticateClient msg
            client     = (clientState, conn)
            disconnect = do
              -- Remove client and return new state
              s <- modifyMVar state $ \s ->
                let s' = removeClient client s in return (s', s')
              infoM $ (clientForLog clientState) ++ " disconnected"

clientForLog (Authorized name idx) = name ++ "(" ++ (show idx) ++ ")"
clientForLog _ = "Unauthorized client"

newServerState :: g -> ServerState g
newServerState x = (x, [])

data Show a => GameServerGame g a = GameServerGame
  { initState :: [String] -> g
  , update :: g -> a -> g 
  , parseAction :: Text -> Either String a
  , serializeGame :: g -> Int -> Text
  }

runServer :: Show a => [String] -> (GameServerGame g a) -> IO ()
runServer playerNames gsg = do
  let
    ip = "0.0.0.0" 
    port = 9160
  updateGlobalLogger rootLogger (setLevel INFO)
  infoM ("Starting server for " ++ (intercalate ", " playerNames))
  infoM ("Binding to " ++ ip ++ ":" ++ (show port))
  state <- newMVar $ newServerState ((initState gsg) playerNames)
  WS.runServer ip port $ application gsg state 

