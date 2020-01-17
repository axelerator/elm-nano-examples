module Main exposing (..)

import Animation exposing (px, deg, turn)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array
import Random

main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = { cards : List Card }

init : ( Model, Cmd Msg )
init = ({ cards = [] }, Cmd.none )

view : Model -> Html Msg
view model =
  div []
    [ div [ style "position" "relative" ] (List.indexedMap cardView model.cards)
    , button [onClick (CreateCard)] [text "AddCard"]
    ]

type Msg
    = CreateCard            -- request a random number for the new cards content
    | AddCard Int           -- add the new card and trigger appropiate animation
    | Animate Animation.Msg -- handle animation progress
    | FocusCard Int         -- react to user hovering cursor over card
    | BlurCard Int          -- reverse effect after user removes curser from card

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FocusCard i ->
            ({ model | cards = markCardActive model.cards i }, Cmd.none)
        BlurCard i ->
            ({ model | cards = markCardInActive model.cards i }, Cmd.none)
        CreateCard ->
            ( model, Random.generate AddCard (Random.int 0 (Array.length cardNames)))
        AddCard i ->
            ({ model | cards = addCard i model.cards }, Cmd.none)
        Animate animMsg ->
            ({ model | cards = List.map (animateCard animMsg) model.cards }, Cmd.none)

-- the animations have to be updated periodically so we have to subscribe to them
subscriptions : Model -> Sub Msg
subscriptions model = Animation.subscription Animate (List.map (\c -> c.style) model.cards)

type alias Card = 
  { cardName : String       -- name being displayed on the card
  , focussed : Bool         -- is this card being pointed at
  , style : Animation.State -- tracks the current state of this cards animation
  }

rgba r g b a = -- convinience helper to create a color
    { red = r, blue = b, green = g, alpha = a }

-- inital position for a card to appear, aka 'on top of drawPile'
drawPilePosition = Animation.style 
  [ Animation.rotate (deg 0.0)
  , Animation.left (px 20), Animation.top (px 100)
  , Animation.scale 1.0
  , Animation.shadow
              { offsetX = 2
              , offsetY = 2
              , size = 2
              , blur = 2
              , color = rgba 0 0 0 0.1
              }
  ]

-- some helper functions to create a card
cardNames = Array.fromList ["Unicorn", "Elephant", "Mouse", "Chicken", "Dragon", "Vampire"]

cardName i =
  case Array.get i cardNames of
    Just name -> name
    Nothing -> "Ghost"

mkCard i = 
  { cardName = cardName i
  , style = drawPilePosition
  , focussed = False
  }

-- calculates the angle in degrees for it i-th card of a total number of cards
cardRotation total i =
  let
    maxSpread = 80.0
    perCard = maxSpread / (toFloat total)
  in
    if total == 1 then
      deg 0.0
    else
      deg (perCard * (toFloat i) - (maxSpread * 0.5))

-- calculate the target position/scale for highlighting the i-th of cardCount cards
targetHandCard : Int -> Int -> Card -> Card
targetHandCard cardCount i card = 
  let
    ani =
      [ Animation.to
          [ Animation.left (px (300.0 + (toFloat i) * 10.0 ))
          , Animation.rotate (cardRotation cardCount i)
          ]
      ]
  in
     { card | style = Animation.interrupt ani card.style }

-- adds a new card to the given list of cards. The int is used to select
-- the content of the new card
addCard : Int -> List Card -> List Card
addCard i cards = 
  let
    cards_ = ((mkCard i) :: cards)
  in
    List.indexedMap (targetHandCard (List.length cards_)) cards_

-- change the card at position 'toFocus' to being focussed and queue the appropriate
-- animation
markCardActive : List Card -> Int -> List Card
markCardActive cards toFocus =
  let
    ani = [ Animation.to [ Animation.scale 1.1 ] ]
  in
    List.indexedMap (\i c -> ( 
      { c 
      | focussed = i == toFocus 
      , style = if i == toFocus then Animation.interrupt ani c.style else c.style
      })) cards

-- revert the highlighting/focussing from the card with the given index
markCardInActive : List Card -> Int -> List Card
markCardInActive cards toFocus =
  let
    ani = [ Animation.to [ Animation.scale 1.0 ] ]
  in
    List.indexedMap (\i c -> ( 
      { c 
      | focussed = i == toFocus 
      , style = if i == toFocus then Animation.interrupt ani c.style else c.style
      })) cards

animateCard : Animation.Msg -> Card -> Card
animateCard animMsg c = { c | style = Animation.update animMsg c.style }

-- view function for a single card at position i
cardView i card = div 
                  (Animation.render card.style
                    ++ [ style "width" "80px"  
                       , style "height" "140px"
                       , style "position" "relative"
                       , style "background-color" "#EEE"
                       , style "position" "absolute"
                       , style "text-align" "center"
                       , style "border" "1px solid gray"
                       , style "border-radius" "2px"
                       , style "transform-origin" "bottom center"
                       , onMouseEnter (FocusCard i)
                       , onMouseLeave (BlurCard i)
                       ]
                    ++ (if card.focussed then [style "z-index" "10"] else [])
                 ) [text card.cardName]

