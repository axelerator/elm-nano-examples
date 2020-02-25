port module GameClient exposing
    ( GameDescription
    , ServerInteractions
    , connect
    , serverInteractions
    )

import Json.Decode
    exposing
        ( Decoder
        , decodeString
        , errorToString
        )
import Json.Encode as Encode


port websocketIn : (String -> msg) -> Sub msg


port websocketOut : String -> Cmd msg


type alias GameDescription game action msg =
    { gameDecoder : Decoder game
    , actionEncoder : action -> Encode.Value
    , playMsg : action -> msg
    , errorMsg : String -> msg
    , updateGameMsg : game -> msg
    }


serverInteractions : GameDescription game action msg -> ServerInteractions action msg model
serverInteractions gameDescription =
    { updateForPlay = play gameDescription.actionEncoder
    , subscriptions = websocketIn (stringToMsg gameDescription)
    }


type alias ServerInteractions action msg model =
    { updateForPlay : model -> action -> ( model, Cmd msg )
    , subscriptions : Sub msg
    }


connect : String -> Cmd msg
connect authToken =
    let
        authJson =
            "{ \"type\": \"Auth\", \"token\": \"" ++ authToken ++ "\"}"
    in
    websocketOut authJson


play : (a -> Encode.Value) -> model -> a -> ( model, Cmd msg )
play encoder model action =
    let
        jsonObject =
            encoder action

        jsonString =
            Encode.encode 0 jsonObject
    in
    ( model, websocketOut jsonString )


stringToMsg : GameDescription game action msg -> String -> msg
stringToMsg gameDescription s =
    case decodeString gameDescription.gameDecoder s of
        Ok clientGame ->
            gameDescription.updateGameMsg clientGame

        Err error ->
            gameDescription.errorMsg (errorToString error)
