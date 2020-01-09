module Game exposing (..)
import Json.Decode exposing (Decoder, map2, field, string, int, list, decodeString, errorToString)

import Html exposing (Html, a, button, div, h1, input, p, span, text)
import Html.Attributes exposing (checked, disabled, href, size, style, type_, value)
import Debug


type alias ClientState =
  { myself : Me
  , opponents : List Opponent
  }

type alias ClientCard = 
  { id : String
  , label : String
  }

type alias Me = 
  { myHand : List ClientCard
  , myBattlefield : List ClientCard
  }

type alias Opponent = 
  { theirHand : Int
  , theirBattlefield : List ClientCard
  }

clientStateDecoder : Decoder ClientState
clientStateDecoder =
  map2 ClientState
    (field "me" meDecoder)
    (field "opponents" (list opponentDecoder))

cardDecoder : Decoder ClientCard
cardDecoder =
  map2 ClientCard
    (field "id" string)
    (field "label" string)

opponentDecoder : Decoder Opponent
opponentDecoder =
  map2 Opponent
    (field "hand" int)
    (field "battlefield" (list cardDecoder))

meDecoder : Decoder Me
meDecoder =
  map2 Me
    (field "hand" (list cardDecoder))
    (field "battlefield" (list cardDecoder))

openCardView : ClientCard -> Html msg
openCardView card = 
  div [ style "border" "solid"
      , style "width" "40px"
      , style "height" "80px"
      ]
      [ text card.label
      ]

opponentView : Opponent -> Html msg
opponentView op =
  div [] 
   [ text ("hand: " ++ (String.fromInt op.theirHand))
   , div [style "display" "flex"] 
         (List.map openCardView op.theirBattlefield )
   ]

mkGame : String -> Maybe ClientState
mkGame json =
  let
    result = decodeString clientStateDecoder json
  in
    case result of
      Ok cs -> Just cs
      _ -> Nothing

gameView : Maybe ClientState -> Html msg
gameView result = 
  case result of
    Just cs ->
      div [] 
        [ div [] [ text "ops"
                 , div [] (List.map opponentView cs.opponents)
                 ]
        , div [] [text "me"
                 , div [style "display" "flex"] 
                       (List.map openCardView cs.myself.myBattlefield )
                 , div [style "display" "flex"] 
                       (List.map openCardView cs.myself.myHand )
                 ]
        ]
    _ ->
      div [] [text "No game"]
        


