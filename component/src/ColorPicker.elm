module ColorPicker exposing (init, value, update, view, EditableColor, Msg)

import Html exposing (Html, text, button, div)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style)

-- the state of the editor
type Model = Intensity Int -- means we're currently editing
           | Finished      -- means the picker is not visible

-- Opaque type that holds the selected color and the state of the editor
-- This will be the type other components will use to store the color in.
-- Opaque means: The outside knows our type: EditableColor
-- but they don't know the constructor HiddenEditableColor. This way they
-- can't tie their implementation to the internals of our type.
type EditableColor = HiddenEditableColor Int Model

-- Constructor function for the outside to create the inital state from
-- a primitive color value - Int in our case
init : Int -> EditableColor
init i = HiddenEditableColor i Finished

-- Accessor function to get the value 'out' of our opqque type
value : EditableColor -> Int
value (HiddenEditableColor i _) = i

type Msg = 
    Brighter
  | Darker
  | Start  -- show the 'picker' UI elements
  | Finish -- use the selected value and hide the picker

-- Updates our picker state with the proper reaction to one of our local
-- picker specific messages.
update : EditableColor -> Msg -> EditableColor
update editableColor msg = 
  let
    (HiddenEditableColor storedColor model) = editableColor
  in
    case model of
      Finished ->
        case msg of 
          Start     -> HiddenEditableColor storedColor (Intensity storedColor)
          _         -> editableColor
      Intensity editorColor ->
        case msg of 
          Start    -> editableColor
          Brighter -> HiddenEditableColor storedColor (Intensity (editorColor + 5))
          Darker   -> HiddenEditableColor storedColor (Intensity (editorColor - 5))
          Finish   -> HiddenEditableColor editorColor Finished

-- next to our model we also need a wrapper function that lifts our local message
-- to the app level
view : EditableColor -> (Msg -> msg) -> Html msg
view (HiddenEditableColor _ model) msgWrapper = 
  case model of
    Finished -> button [onClick (msgWrapper Start)] [text "edit"]
    Intensity i ->
      div [] [
          button [ onClick (msgWrapper Darker) ] [ text "-" ]
        , text (String.fromInt i)
        , button [ onClick (msgWrapper Brighter) ] [ text "+" ]
        , button [ onClick (msgWrapper Finish) ] [ text "Done" ]
      ]

