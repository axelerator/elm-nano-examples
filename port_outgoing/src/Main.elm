port module Main exposing (..)

import Browser
import Html exposing (Html, text, button)
import Html.Events exposing (onClick)

-- PORT
-- The first line in this file is super important to actually expose the port!
-- In this line we name one specific port. "outgoingPort" is an identifier that
-- can be chosen freely. It is the name to which we can subscribe (see below)
-- and by which we will reference the port from the outside.
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

type Model = ModelState


init : () -> (Model, Cmd Msg)
init _ = ( ModelState, Cmd.none )

-- UPDATE


type Msg = SendPort 

update : Msg -> Model -> (Model, Cmd Msg)
update _ model = (model, outgoingPort "from Elm")

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Html Msg
view model = button [ onClick SendPort ] [ text "Elm button" ]

