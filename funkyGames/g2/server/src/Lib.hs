module Lib
    ( Game(..)
    , Player(..)
    , Card(..)
    , Sign(..)
    , cardsWithLocation
    , LocatedCard
    , Place(..)
    , PlayerId
    , PlayerAction(..)
    , aGame
    , drawCard
    , playCard
    , availableActionsFor
    ) where

import Utils

data Game = Game
  { players :: [Player]
  , turn :: Int
  , drawPile :: [Card]
  , discardPile :: [Card]
  } deriving (Show)

data Player = Player
  { name :: String
  , hand :: [Card]
  } deriving (Show)

data PlayerAction
  = PlayCard Int
  | DrawCard
  deriving (Show, Eq)

data Card = Card 
  { cardColor :: Color 
  , cardSign :: Sign
  , cardId :: CardId 
  }

type CardId = String

type PlayerId = Int

data Sign = Number Int | Jack
  deriving (Show, Eq)

data Color = Red | Blue
  deriving (Show, Eq)

data Place
  = Hand PlayerId
  | DrawPile
  | DiscardPile
  deriving (Show)

type LocatedCard = (Card, Place, Int)

instance Show Card where
  show (Card color (Number i) _) = colorize color (show i)
  show (Card color Jack _) = colorize color ("Jack")

addIds :: [(CardId -> Card)] -> [Card]
addIds cs =
  let
    withIdx = zip cs [0..]
    frmTpl (cc, i) = cc (show i)
  in
    map frmTpl withIdx

deck :: IO [Card]
deck = 
  let
    numbersWithCol c = map (\i -> Card c (Number i)) [1..7]
    numbers = concat $ map numbersWithCol [Red, Blue]
    jacks = map (\c -> Card c Jack) [Red, Blue]
  in
    shuffle $ addIds (numbers ++ jacks)

aGame :: IO Game
aGame = do
   mixedDeck <- deck
   case mixedDeck of
     _ -> return $ Game [p1, p2] 0 drawP discardP  
      where
        (h1, rest) = splitAt 5 mixedDeck
        (h2, rest') = splitAt 5 rest
        (discardP, drawP) = splitAt 1 rest'
        p1 = Player "Axel" h1
        p2 = Player "Laurine" h2


mkIndexed :: Place -> [Card] -> [LocatedCard]
mkIndexed place cards =
  let
    mkIdx (c, i) = (c, place, i)
  in
    map mkIdx (zip cards [0..])

cardsWithLocation :: Game -> [LocatedCard]
cardsWithLocation game =
  let
    drawPileCards = mkIndexed DrawPile (drawPile game)
    discardPileCards = mkIndexed DiscardPile (discardPile game)
    handCardsOf i = hand ((players game) !! i)
    playersCards pI = mkIndexed (Hand pI) (handCardsOf pI) 

    handCards = concat $ map playersCards [0..(length (players game)) - 1]
  in
    concat
      [ drawPileCards
      , discardPileCards
      , handCards
      ]

giveCardsToPlayer :: Game -> PlayerId -> [Card] -> Game
giveCardsToPlayer game playerId cards =
  let
    playerWithIds = zip (players game) [0..]
    updatePlayer (p, pId) = if pId == playerId then
                              p { hand = cards ++ (hand p) }
                            else
                              p
  in
    game { players = map updatePlayer playerWithIds }

drawCard :: Game -> PlayerId -> Game
drawCard game playerId =
  let
    (c, rest) = splitAt 1 (drawPile game)
  in
    if null c then
      game
    else
      (giveCardsToPlayer game playerId c) { drawPile = rest }

canPlayCard :: Game -> PlayerId -> Int -> Bool
canPlayCard game playerId handIdx =
  let
    desiredAction = PlayCard handIdx
    availableActions = availableActionsFor game playerId
  in
    elem desiredAction availableActions

playCard :: Game -> PlayerId -> Int -> Game
playCard game playerId idx =
  let
    player = (players game) !! playerId
    playersHand = hand player
    (card, playersHandWithoutCard) = withoutNth playersHand idx
    newDiscardPile = card:(discardPile game)
    playerWithoutCard = player { hand = playersHandWithoutCard  }
    updatedPlayers = replaceNth (players game) playerId playerWithoutCard
  in
    if canPlayCard game playerId idx then
      game
        { players = updatedPlayers
        , discardPile = newDiscardPile
        , turn = ((turn game) + 1) `mod` (length $ players game)
        }
    else
      game

playableCards :: Card -> [(Card, Int)] -> [(Card, Int)]
playableCards (Card curCol curSign _) availableCs =
  let
    match ((Card col sign _), _) = col == curCol || sign == curSign
  in
    filter match availableCs 

availableActionsFor :: Game -> PlayerId -> [PlayerAction]
availableActionsFor game playerId =
  let
    topCard = if null (discardPile game) then
                Nothing
              else
                Just $ head (discardPile game)
    player = (players game) !! playerId
    indexedHand = zip (hand player) [0..]
  in
    if (turn game) /= playerId then
      []
    else
      case topCard of
        Just c -> map (\(_, i) -> PlayCard i) (playableCards c indexedHand)
        Nothing -> map PlayCard [0..(length $ hand player) - 1]

colorize :: Color -> String -> String
colorize c s =
  let
    colorNum Red = "32"
    colorNum Blue = "34"
  in
    "\x1b[" ++ (colorNum c) ++ "m" ++ (s) ++ "\x1b[m" 


