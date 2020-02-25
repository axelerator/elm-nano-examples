{-# LANGUAGE OverloadedStrings #-}

module FizzBuzz 
  ( fizzBuzz
  ) where

import Data.Aeson
import Data.Text (Text)

import JsonUtils
import GameServer as GS

data FizzBuzzState 
  = FizzBuzzState 
    { number :: Int
    , players :: [Int]
    }

data FizzBuzzClientState = FizzBuzzClientState Int Bool Int

instance ToJSON FizzBuzzClientState where
  toJSON (FizzBuzzClientState num myTurn losses) = 
    object [ "type" .= ("FizzBuzzClientState"::Text)
           , "number" .= num 
           , "myTurn" .= myTurn
           , "losses" .= losses
           ]

data FizzBuzzAction 
  = Num Int
  | Fizz
  | Buzz
  | FizzBuzz
  deriving (Eq, Show)

instance FromJSON FizzBuzzAction where
  parseJSON = withObject "Action" $ \o -> do
    msgKind <- o .: "type"
    case msgKind of
      "Fizz" -> return Fizz
      "Buzz" -> return Buzz
      "FizzBuzz" -> return FizzBuzz
      "Num" -> Num <$> o .: "num" 
      _ -> fail $ msgKind ++ " is not a valid action type"

initFB playerNames = FizzBuzzState 0 [0,0]

lostGame oldGame lostPlayerId =
  let
    increaseLoser (before, pId) = if pId == lostPlayerId then before + 1 else before
    newPlayers = map increaseLoser (zip (players oldGame) [0..])
  in
    FizzBuzzState 0 newPlayers

updateFB :: FizzBuzzState -> FizzBuzzAction -> FizzBuzzState
updateFB currentGame@(FizzBuzzState lastNum ps) a =
  let
    num = lastNum + 1
    continuedGame = FizzBuzzState num ps
    currentPlayerId = lastNum `mod` (length ps)
    expectedAction =
      if (num `mod` 3) == 0 then
        if (num `mod` 5) == 0 then
          FizzBuzz
        else
          Fizz
      else
        if (num `mod` 5) == 0 then
          Buzz
        else
          Num num
  in
    if a == expectedAction then
      continuedGame
    else
      lostGame currentGame currentPlayerId
  

parseFBAction :: Text -> Either String FizzBuzzAction
parseFBAction text = fromJsonText text

toClientState :: FizzBuzzState -> Int -> FizzBuzzClientState
toClientState (FizzBuzzState lastN players) playerId =
  FizzBuzzClientState lastN ((lastN `mod` (length players)) == playerId) (players !! playerId)

fBGameToJson :: FizzBuzzState -> Int -> Text
fBGameToJson g playerId = toJsonText (toClientState g playerId)

fizzBuzz = GS.GameServerGame
  { initState = initFB
  , update = updateFB
  , parseAction = parseFBAction
  , serializeGame = fBGameToJson
  }


