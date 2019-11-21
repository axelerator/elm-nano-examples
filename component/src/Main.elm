import Browser
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (style)

import ColorPicker 
import Debug exposing (log)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

-- our model is just a container for two color values, each wrapped in 
-- the type that will also contain the picker state
type Model = Colors ColorPicker.EditableColor ColorPicker.EditableColor

init : () -> (Model, Cmd Msg)
init _ = ( (Colors (ColorPicker.init 125) (ColorPicker.init 200) ), Cmd.none )

-- UPDATE

-- next to our regular messages we need also to 'transport' all the messages
-- send by the picker component.
-- Since we have two pickers we need two messages to be able to forward the
-- messages to the right picker
type Msg = None
         | EditCol1 ColorPicker.Msg
         | EditCol2 ColorPicker.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg (Colors color1 color2) = 
  case msg of
    EditCol1 subMsg -> (Colors (ColorPicker.update color1 subMsg) color2, Cmd.none)
    EditCol2 subMsg -> (Colors color1 (ColorPicker.update color2 subMsg), Cmd.none)
    None -> ((Colors color1 color2), Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

colorView : (ColorPicker.Msg -> Msg) -> ColorPicker.EditableColor -> Html Msg
colorView wrapper editableColor =
  let
    -- we extract the actual color value with the function provided by
    -- the picker component
    intensity = ColorPicker.value editableColor
    intensityAsStr = String.fromInt intensity
    rgbStr = String.join "," [intensityAsStr, intensityAsStr, intensityAsStr]
    backgroundColorStyle = style "background-color" ("rgb(" ++ rgbStr ++ ")")
  in
    div [] [
      -- we combine our 'display' of the component with the
      -- editor view provided by the picker component
      div [backgroundColorStyle] [text intensityAsStr]
    , ColorPicker.view editableColor wrapper
    ] 

view : Model -> Html Msg
view (Colors editableColor1 editableColor2) =
  div [] [ colorView EditCol1 editableColor1
         , colorView EditCol2 editableColor2
         ]

