module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (rect, shapes, toHtml)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


initialModel : Model
initialModel =
    { x = 100
    , y = 100
    , vx = 0
    , vy = 0
    }


type Msg
    = KeyDown String
    | KeyUp String
    | Tick Float


speed : Float
speed =
    200


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            case String.toLower key of
                "w" ->
                    ( { model | vy = -1 }, Cmd.none )

                "s" ->
                    ( { model | vy = 1 }, Cmd.none )

                "a" ->
                    ( { model | vx = -1 }, Cmd.none )

                "d" ->
                    ( { model | vx = 1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            case String.toLower key of
                "w" ->
                    ( { model | vy = 0 }, Cmd.none )

                "s" ->
                    ( { model | vy = 0 }, Cmd.none )

                "a" ->
                    ( { model | vx = 0 }, Cmd.none )

                "d" ->
                    ( { model | vx = 0 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick dt ->
            let
                dtSeconds =
                    dt / 1000
            in
            ( { model
                | x = model.x + model.vx * speed * dtSeconds
                , y = model.y + model.vy * speed * dtSeconds
              }
            , Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text
                ("x: "
                    ++ String.fromInt (round model.x)
                    ++ ", y: "
                    ++ String.fromInt (round model.y)
                )
            ]
        , toHtml
            ( 600, 400 )
            [ style "background-color" "#ccffcc"
            , style "border" "9px solid black"
            , style "display" "block"
            ]
            [ shapes [ fill Color.lightGray ]
                [ rect ( 0, 0 ) 600 400 ]
            , shapes [ fill Color.blue ]
                [ rect ( model.x, model.y ) 50 50 ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
        , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
        , Browser.Events.onAnimationFrameDelta Tick
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }