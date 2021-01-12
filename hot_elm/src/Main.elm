module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import String exposing (fromInt)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { left : Int
    , right : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { left = 0, right = 0 }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Left
    | Right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Left ->
            ( { model | left = model.left + 1 }
            , Cmd.none
            )

        Right ->
            ( { model | right = model.right + 1 }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Left ] [ text "Left: ", text <| fromInt model.left ]
        , button [ onClick Right ] [ text "Right: ", text <| fromInt model.right ]
        , div [] [ text "LOOK MUM, NO SERVER" ]
        ]
