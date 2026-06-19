module Learning exposing (main)


-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW
hib4 : String -> String
hib4 x = "hi " ++ x
    
-- hib4rev : String -> String
-- hib4rev x = hib4(String.reverse(x))

-- ^^ this current one reverses and then applies hib4
-- new one: apply hib4, then reverse

revhib4 : String -> String
revhib4 x = String.reverse(hib4(x))


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
        , div [] [ text (revhib4 model.content) ]
        ]