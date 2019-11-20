port module Main exposing (..)

import Browser
import Html exposing (Html, text, pre)

-- PORT
-- The first line in this file is super important to actually expose the port!
-- In this line we name one specific port. "incomingPort" is an identifier that
-- can be chosen freely. It is the name to which we can subscribe (see below)
-- and by which we will reference the port from the outside.
port incomingPort : (String -> msg) -> Sub msg

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPort fromPort -> (Full fromPort, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = incomingPort GotPort

-- VIEW

view : Model -> Html Msg
view model =
  case model of
    Empty ->
      text "Nothing received yet"
    Full fromPort ->
      text ("Received the call through the port:" ++ fromPort)

