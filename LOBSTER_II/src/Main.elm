module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (path, rect, shapes, toHtml, lineTo)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Line exposing (lineWidth)
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode

type alias Model =
    { x : Float, y : Float, vx : Float, vy : Float }

initialModel : Model
initialModel = { x = 100, y = 100, vx = 0, vy = 0 }

type Msg = KeyDown String | KeyUp String | Tick Float

speed : Float
speed = 200

playerSize : Float
playerSize = 50

screenW : Int
screenW = 600

screenH : Int
screenH = 400

axisThickness : Float
axisThickness = 1

bushes : List ( Float, Float )
bushes = [ ( 200, 200 ), ( 400, 150 ), ( 300, 350 ) ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown k -> ( handleDown k model, Cmd.none )
        KeyUp k -> ( handleUp k model, Cmd.none )
        Tick dt -> ( step dt model, Cmd.none )

handleDown : String -> Model -> Model
handleDown k m =
    case String.toLower k of
        "w" -> setVy m -1
        "s" -> setVy m 1
        "a" -> setVx m -1
        "d" -> setVx m 1
        _ -> m

handleUp : String -> Model -> Model
handleUp k m =
    case String.toLower k of
        "w" -> setVy m 0
        "s" -> setVy m 0
        "a" -> setVx m 0
        "d" -> setVx m 0
        _ -> m

setVx : Model -> Float -> Model
setVx m v = { m | vx = v }

setVy : Model -> Float -> Model
setVy m v = { m | vy = v }

step : Float -> Model -> Model
step dt m =
    let t = dt / 1000 in
    { m | x = m.x + m.vx * speed * t, y = m.y + m.vy * speed * t }

centerX : Float
centerX = toFloat screenW / 2

centerY : Float
centerY = toFloat screenH / 2

camX : Model -> Float -> Float
camX m wx = centerX + (wx - m.x)

camY : Model -> Float -> Float
camY m wy = centerY + (wy - m.y)

posText : Model -> String
posText m =
    "x: " ++ String.fromInt (round m.x) ++ ", y: " ++ String.fromInt (round m.y)

view : Model -> Html Msg
view m =
    div [ style "display" "flex", style "gap" "10px" ]
        [ div [] [ text (posText m), worldCanvas m ]
        , debugCanvas m
        ]

worldCanvas : Model -> Html Msg
worldCanvas m =
    toHtml ( screenW, screenH ) worldCanvasStyles (worldScene m)

debugCanvas : Model -> Html Msg
debugCanvas m =
    toHtml ( screenW, screenH ) debugCanvasStyles (debugScene m)

worldCanvasStyles : List (Html.Attribute Msg)
worldCanvasStyles =
    [ style "background-color" "#ccffcc"
    , style "border" "3px solid black"
    , style "display" "block"
    ]

debugCanvasStyles : List (Html.Attribute Msg)
debugCanvasStyles =
    [ style "background-color" "#ffffff"
    , style "border" "3px solid black"
    , style "display" "block"
    ]

worldScene : Model -> List Canvas.Renderable
worldScene m =
    background m :: bushesView m ++ [ playerView ]

debugScene : Model -> List Canvas.Renderable
debugScene m =
    background m :: bushesView m ++ [ axesView m ] ++ [ playerView ]

background : Model -> Canvas.Renderable
background _ =
    shapes [ fill Color.lightGray ]
        [ rect ( 0, 0 ) (toFloat screenW) (toFloat screenH) ]

bushesView : Model -> List Canvas.Renderable
bushesView m =
    List.map (bushView m) bushes

bushView : Model -> ( Float, Float ) -> Canvas.Renderable
bushView m ( bx, by ) =
    shapes [ fill Color.green ]
        [ rect ( camX m bx, camY m by ) 20 20 ]

playerView : Canvas.Renderable
playerView =
    shapes [ fill Color.blue ]
        [ rect ( centerX - playerSize / 2, centerY - playerSize / 2 )
            playerSize
            playerSize
        ]



-- source:  https://github.com/joakin/elm-canvas/blob/5.0.0/examples/TiledLines.elm
drawLine : Canvas.Point -> Canvas.Point -> Canvas.Shape
drawLine start end = path start [ lineTo end ]


axesView : Model -> Canvas.Renderable
axesView m = shapes []
    [ drawLine (camX m -2000, camY m 0) (camX m 2000, camY m 0)
    , drawLine (camX m 0, camY m -2000) (camX m 0, camY m 2000) ]

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