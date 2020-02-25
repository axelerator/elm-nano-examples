{-# LANGUAGE OverloadedStrings #-}
module Client
    ( Client.Game
    , gameForPlayers
    ) where

import Data.Maybe (catMaybes) 
import Data.Aeson
import qualified Data.Text as T

import Lib as G

data CardDetails
  = Hidden
  | Open G.Card
  deriving (Show)

data Card = Card
  { cardId :: String
  , location :: Location
  , details :: CardDetails
  }
  deriving (Show)

data Game = Game
  { cards :: [Client.Card]
  , drawPileSize :: Int
  , opponentIds :: [G.PlayerId]
  , currentPhase :: Phase
  , availableActions :: [G.PlayerAction]
  }
  deriving (Show)

data Phase
  = MyTurn
  | TheirTurn Int
  deriving (Show, Eq)


data Stack 
  = MyHand
  | TheirHand Int
  | DiscardPile
  deriving (Show)

type Location = (Stack, Int)

signForJson (G.Number i) = show i
signForJson (G.Jack) = "J"

instance ToJSON Phase where
  toJSON MyTurn = object [ "type" .= ("MyTurn"::T.Text) ]
  toJSON (TheirTurn i) = 
    object 
      [ "type" .= ("TheirTurn"::T.Text)
      , "opponentId" .= i
      ]

instance ToJSON G.PlayerAction where
  toJSON DrawCard = object [ "type" .= ("DrawCard"::T.Text) ]
  toJSON (PlayCard i) = 
    object 
      [ "type" .= ("PlayCard"::T.Text)
      , "handIdx" .= i
      ]

instance ToJSON CardDetails where
  toJSON Hidden = object [ "type" .= ("Hidden" :: T.Text)] 
  toJSON (Open card) = 
    object 
      [ "type" .= ("Open"::T.Text)
      , "color" .= (show $ cardColor card)
      , "sign" .= (signForJson $ cardSign card)
      ] 

instance ToJSON Client.Card where
  toJSON c = 
    let
      (stack, idx) = location c
    in
      object 
        [ "cardId" .= Client.cardId c
        , "location" .= 
          object 
            [ "stack" .= stack
            , "idx" .= idx
            ]
        , "details" .= details c
        ] 

instance ToJSON Client.Game where
  toJSON game = 
    object
      [ "cards" .= cards game
      , "drawPileSize" .= drawPileSize game
      , "opponentIds" .= opponentIds game
      , "phase" .= currentPhase game
      , "availableActions" .= availableActions game
      ]

instance ToJSON Stack where
  toJSON Client.MyHand = object [ "type" .= ("MyHand"::T.Text) ]
  toJSON (Client.TheirHand i) = 
    object 
      [ "type" .= ("TheirHand"::T.Text)
      , "playerId" .= i
      ]
  toJSON Client.DiscardPile = 
    object 
      [ "type" .= ("DiscardPile"::T.Text)
      ]

cardForPlayer :: PlayerId -> LocatedCard -> Maybe Client.Card
cardForPlayer myId (card, place, idx) =
  case place of
    G.DrawPile -> Nothing
    G.DiscardPile -> Just (Client.Card (G.cardId card) (Client.DiscardPile, idx) (Open card))
    G.Hand playerId ->
      if myId == playerId then
        Just (Client.Card (G.cardId card) (MyHand, idx) (Open card))
      else
        Just (Client.Card (G.cardId card) (TheirHand playerId, idx) Hidden)

cardsForPlayer :: [LocatedCard] -> PlayerId -> (PlayerId, [Client.Card])
cardsForPlayer cards playerId =
  (playerId, catMaybes $ map (cardForPlayer playerId) cards)


gameForPlayers :: G.Game -> [(G.PlayerId, Client.Game)]
gameForPlayers game =
  let
    drawPileSize = length $ G.drawPile game
    cwls = cardsWithLocation game
    playerIds = [0..(length . G.players $ game) - 1] 
    cardsByPlayer = map (cardsForPlayer cwls) playerIds
    opponentIds myId = filter ((/=) myId) playerIds 
    currentTurn pId = if (turn game) == pId then MyTurn else TheirTurn (turn game)
    actions pId = G.availableActionsFor game pId 
    mkGame (pId, cs) = (pId, Client.Game cs drawPileSize (opponentIds pId) (currentTurn pId) (actions pId))
  in
    map mkGame cardsByPlayer
   
