{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( initGame
    , gameT1
    , Game
    , forClient
    ) where

import Data.Aeson

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Action = Auth { token :: String }
            | Send { message :: String }

instance FromJSON Action  where
  parseJSON = withObject "Action" $ \o -> do
    actionKind <- o .: "action"
    case actionKind of
      "Auth" -> Auth <$> o .: "token" 
      "Send" -> Send <$> o .: "message" 
      _        -> fail ("unknown kind: " ++ actionKind)


data Operator = Plus | Minus
  deriving (Eq, Show)

type CardId = Integer

data CardPrototype =
    OperatorCard Operator
  | NumberCard Int

type Card = (CardId, CardPrototype)

type PlayerId = Int

data Stack = 
    Deck 
  | Hand PlayerId
  | Battlefield PlayerId
  | Discarded

type Location = (Stack, Int)

type Player = 
  ( [Card] -- hand
  , [Card] -- battlefield
  )

data GameAction = StartGame

data Effect =
    DrawCard PlayerId
--  | RevealFromHand
--  | PlaceOnBattleField
--  | Discard

data PlayerAction = PlayCard Int
                  | Pass

data Game = Game { drawPile :: [Card]
                 , players :: [Player]
                 , aboutToBePlayed :: Maybe Card
                 , discarded :: [Card]
                 , fxQueue :: [Effect]
                 , fxHistory :: [Effect]
                 }

initDeck :: [Card]
initDeck = zip [0..] (concat
  [ take 5 (repeat (OperatorCard Plus ))
  , take 5 (repeat (OperatorCard Minus ))
  , take 9 (map NumberCard [1..10] ) 
  , take 9 (map NumberCard [1..10] ) 
  , take 9 (map NumberCard [1..10] ) 
  ])

initPlayer = ( [], [] )

drawCards :: Int -> PlayerId -> [Effect]
drawCards count playerId = take count (repeat (DrawCard playerId))

initFx = concat $ map (drawCards 4) [0..(playerCount-1)]

playerCount = 2

initGame = Game 
  initDeck
  (take playerCount (repeat initPlayer))
  Nothing  -- aboutToBePlayed
  []       -- discarded
  []   -- fxQ
  []       -- fxHistory

gameT1 :: Game
gameT1 = head $ processAction initGame StartGame

allEffectsProcessed :: Game -> Bool
allEffectsProcessed = null . fxQueue

processAction :: Game -> GameAction -> [Game]
processAction game StartGame = 
  let
    fx = initFx
    gameWithFx = game { fxQueue = fx }
  in
    processAllFx gameWithFx

processAllFx :: Game -> [Game]
processAllFx game = processAllFx' game []

processAllFx' :: Game -> [Game] -> [Game]
processAllFx' currentGame accu =
  let
    nextGame = step currentGame
  in
    if allEffectsProcessed currentGame then
      accu
    else
      processAllFx' (step currentGame) (currentGame:accu)

step :: Game -> Game
step game =
  let
    nextEffect:unprocessedFX = fxQueue game 
    affectedGame = processEffect game nextEffect
    updatedGame = affectedGame { fxQueue = unprocessedFX, fxHistory = (nextEffect:(fxHistory game)) } 
  in
    if allEffectsProcessed game then
      game
    else
      updatedGame

replaceNth xs n newElement = take n xs ++ [newElement] ++ drop (n + 1) xs

findPlayerById :: Game -> PlayerId -> Player
findPlayerById game playerId = (players game) !! playerId

addToHand :: Player -> Card -> Player
addToHand (hand, battlefield) card = ((card:hand), battlefield) 

addCardToHand :: Game -> PlayerId -> Card -> [Player]
addCardToHand game playerId card =
  let
    oldPlayer = findPlayerById game playerId
    updatedPlayer = addToHand oldPlayer card
    newPlayers = replaceNth (players game) playerId updatedPlayer
  in
    newPlayers

processEffect :: Game -> Effect -> Game
processEffect game (DrawCard playerId) =
  let
    -- TODO: check precondition
    (topCard:restDrawPile) = drawPile game
    newPlayers = addCardToHand game playerId topCard
  in
    game { drawPile = restDrawPile, players = newPlayers }


data ClientState =
  ClientState { myself :: Me
              , opponents :: [Opponent]
              }
data ClientCard = 
  ClientCard { id :: String
             , label :: String
             }

data Me = 
  Me { myHand :: [ClientCard]
     , myBattleField :: [ClientCard]
     }

data Opponent = 
  Opponent { theirHand :: Int
           , theirBattleField :: [ClientCard]
           }

mkClientCard :: Card -> ClientCard
mkClientCard (cardId, (OperatorCard op)) = ClientCard (show cardId) (show op)
mkClientCard (cardId, (NumberCard i)) = ClientCard (show cardId) (show i)

mkOpponent :: Player -> Opponent
mkOpponent (hand, battlefield) = 
  let
    clientHand = length hand
    clientBattlefield = map mkClientCard battlefield
  in
    Opponent clientHand clientBattlefield

forClient :: Game -> PlayerId -> ClientState
forClient game playerId =
  let
    (myHand, myBattleField) = findPlayerById game playerId
    allPlayers = players game
    me = Me (map mkClientCard myHand) (map mkClientCard myBattleField)
    otherPlayers = take playerId allPlayers ++  drop (playerId + 1) allPlayers
    them = map mkOpponent otherPlayers
  in
    ClientState me them
              
instance ToJSON Me where
  toJSON (Me hand battlefield) = object [
      "hand" .= hand
    , "battlefield" .= battlefield
    ]
              
instance ToJSON Opponent where
  toJSON (Opponent hand battlefield) = object [
      "hand" .= hand
    , "battlefield" .= battlefield
    ]

instance ToJSON ClientCard where
  toJSON (ClientCard id label) = object [
      "id" .= id
    , "label" .= label
    ]

instance ToJSON ClientState where
  toJSON (ClientState me opponents) = object [
      "me" .= me
    , "opponents" .= opponents
    ]

