port module Main exposing (..)

import Browser
import Html exposing (Html, text, button, div)
import Html.Events exposing (onClick)

port incomingPort : (String -> msg) -> Sub msg
port outgoingPort : String -> Cmd msg

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type Model
  = Empty
  | Full String


init : () -> (Model, Cmd Msg)
init _ = ( Empty, Cmd.none )

-- UPDATE


type Msg = GotPort String
         | SendPort

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPort fromPort -> (Full fromPort, Cmd.none)
    SendPort -> (model, outgoingPort "from Elm")

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = incomingPort GotPort

-- VIEW
buttonView : Model -> Html Msg
buttonView model = button [ onClick SendPort ] [ text "Elm button" ]

valueView : Model -> Html Msg
valueView model =
  case model of
    Empty ->
      text "Nothing received yet"
    Full fromPort ->
      text ("Received the call through the port:" ++ fromPort)

view : Model -> Html Msg
view model =
  div [] [ valueView model
         , buttonView model
         ]

