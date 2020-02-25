module Main exposing (..)

import Browser
import Game as FizzBuzz
import GameClient as GC
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



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
    = Empty String
    | Error String
    | Full FizzBuzz.State


init : String -> ( Model, Cmd Msg )
init authToken =
    ( Empty authToken, Cmd.none )


gameDescription : GC.GameDescription FizzBuzz.State FizzBuzz.Action Msg
gameDescription =
    { gameDecoder = FizzBuzz.fizzBuzzDecoder
    , actionEncoder = FizzBuzz.actionEncoder
    , playMsg = Play
    , updateGameMsg = UpdateGame
    , errorMsg = ServerError
    }


serverInteractions =
    GC.serverInteractions gameDescription



-- UPDATE


type Msg
    = Play FizzBuzz.Action
    | UpdateGame FizzBuzz.State
    | ServerError String
    | Connect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateGame updatedGame ->
            ( Full updatedGame, Cmd.none )

        ServerError e ->
            ( Error e, Cmd.none )

        Connect ->
            case model of
                Empty authToken ->
                    ( model, GC.connect authToken )

                _ ->
                    ( Error "Cant authenticate", Cmd.none )

        Play action ->
            serverInteractions.updateForPlay model action



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    serverInteractions.subscriptions



-- VIEW


view : Model -> Html Msg
view model =
    let
        content =
            case model of
                Empty _ ->
                    text "Nothing received yet"

                Error e ->
                    text e

                Full fbState ->
                    FizzBuzz.view Play fbState
    in
    div []
        [ button [ onClick Connect ] [ text "Connect" ]
        , content
        ]
