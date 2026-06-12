module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (path, rect, shapes, lineTo, Renderable, Point, Shape)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode

type alias Model =
    { x : Float, y : Float, vx : Float, vy : Float }

initialModel : Model
initialModel = { x = 100, y = 100, vx = 0, vy = 0 }

type alias WorldU = Float
type alias Vel = Float
type alias CU = Float
type alias Ms = Float
type alias DistPerMs = Float


type Msg = KeyDown String | KeyUp String | Tick Float

speed : DistPerMs
speed = 0.2

playerSize : CU
playerSize = 50

canvW : Int
canvW = 600

canvH : Int
canvH = 400

bushes : List Point
bushes = [ ( 200, 200 ), ( 400, 150 ), ( 300, 350 ) ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown k -> ( handleDown k model, Cmd.none )
        KeyUp k -> ( handleUp k model, Cmd.none )
        Tick dt -> ( timestep dt model, Cmd.none )

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

setVx : Model -> Vel -> Model
setVx m v = { m | vx = v }

setVy : Model -> Vel -> Model
setVy m v = { m | vy = v }

timestep : Ms -> Model -> Model
timestep dt m = { m | x = m.x + m.vx * speed * dt, y = m.y + m.vy * speed * dt }

centerX : CU
centerX = toFloat canvW / 2

centerY : CU
centerY = toFloat canvH / 2

camX : Model -> WorldU -> CU
camX m wx = centerX + (wx - m.x)

camY : Model -> WorldU -> CU
camY m wy = centerY + (wy - m.y)

posText : Model -> String
posText m =
    "x: " ++ String.fromInt (round m.x) ++ ", y: " ++ String.fromInt (round m.y)

view : Model -> Html Msg
view m =
    div []
        [ div canvWrapperStyles [ worldCanvas m, tabCanvas m ]
        , p [] [text (posText m) ]
        ]

canvWrapperStyles : List (Html.Attribute Msg)
canvWrapperStyles = [ style "display" "flex", style "gap" "10px" ]

worldCanvas : Model -> Html Msg
worldCanvas m =
    Canvas.toHtml ( canvW, canvH ) worldCanvasStyles (worldScene m)

tabCanvas : Model -> Html Msg
tabCanvas m =
    Canvas.toHtml ( canvW, canvH ) tabCanvasStyles (tabScene m)

worldCanvasStyles : List (Html.Attribute Msg)
worldCanvasStyles =
    [ style "background-color" "#ccffcc"
    , style "border" "3px solid black"
    , style "display" "block"
    ]

tabCanvasStyles : List (Html.Attribute Msg)
tabCanvasStyles =
    [ style "background-color" "#ffffff"
    , style "border" "3px solid black"
    , style "display" "block"
    ]

worldScene : Model -> List Renderable
worldScene m = background m ++ bushesView m ++ playerView

tabScene : Model -> List Renderable
tabScene m =
    background m ++ bushesView m ++ axesView m ++ playerView

background : Model -> List Renderable
background _ = [ shapes [ fill Color.lightGray ]
                        [ rect ( 0, 0 ) (toFloat canvW) (toFloat canvH) ] ]

bushesView : Model -> List Renderable
bushesView m = List.map (bushView m) bushes

bushView : Model -> ( Float, Float ) -> Renderable
bushView m ( bx, by ) =
    shapes [ fill Color.green ]
        [ rect ( camX m bx, camY m by ) 20 20 ]

playerView : List Renderable
playerView = [ shapes [ fill Color.blue ]
                      [ rect ( centerX - playerSize / 2, centerY - playerSize / 2 ) playerSize playerSize ] ]

drawLine : Point -> Point -> Shape    -- source:  joakin elm-canvas TiledLines
drawLine start end = path start [ lineTo end ]


axesView : Model -> List Renderable
axesView m = [ shapes [] [ drawLine (camX m -2000, camY m 0) (camX m 2000, camY m 0)
                         , drawLine (camX m 0, camY m -2000) (camX m 0, camY m 2000) ] ]

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