module Game exposing (..)

import Html exposing (Html, a, button, div, h1, h2, h3, input, p, span, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Json.Decode
    exposing
        ( Decoder
        , bool
        , field
        , int
        , map3
        )
import Json.Encode as Encode


type State
    = State Int Bool Int


type Action
    = Num Int
    | Fizz
    | Buzz
    | FizzBuzz
    | WTF String


fizzBuzzDecoder : Decoder State
fizzBuzzDecoder =
    map3 State (field "number" int) (field "myTurn" bool) (field "losses" int)


actionEncoder : Action -> Encode.Value
actionEncoder action =
    case action of
        Num i ->
            Encode.object
                [ ( "type", Encode.string "Num" )
                , ( "num", Encode.int i )
                ]

        Fizz ->
            Encode.object
                [ ( "type", Encode.string "Fizz" ) ]

        FizzBuzz ->
            Encode.object
                [ ( "type", Encode.string "FizzBuzz" ) ]

        Buzz ->
            Encode.object
                [ ( "type", Encode.string "Buzz" ) ]

        _ ->
            Encode.object
                [ ( "type", Encode.string "WTF" ) ]


view playMsg fbState =
    case fbState of
        State lastNum myTurn losses ->
            let
                nextNumStr =
                    String.fromInt (lastNum + 1)

                onClickSendAction a =
                    onClick (playMsg a)

                buttonState =
                    disabled (not myTurn)
            in
            div []
                [ h2 [] [ text (String.fromInt lastNum) ]
                , h3 [] [ text ("You lost: " ++ String.fromInt losses ++ " times") ]
                , div []
                    [ button [ onClickSendAction (Num (lastNum + 1)), buttonState ] [ text nextNumStr ]
                    , button [ onClickSendAction Fizz, buttonState ] [ text "FIZZ!" ]
                    , button [ onClickSendAction Buzz, buttonState ] [ text "BUZZ!" ]
                    , button [ onClickSendAction FizzBuzz, buttonState ] [ text "FIZZ BUZZ!" ]
                    ]
                ]
