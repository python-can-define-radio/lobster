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
    { x : Float, y : Float, vx : Float, vy : Float, lobs : List Lob }

initialModel : Model
initialModel = { x = 100, y = 100, vx = 0, vy = 0, lobs = [] }

type alias Lob = { source: Point, target: Point }

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

move : Ms -> Model -> Model
move dt m = { m | x = m.x + m.vx * speed * dt, y = m.y + m.vy * speed * dt }

timestep : Ms -> Model -> Model
timestep dt m = recordLob (move dt m)

centerX : CU
centerX = toFloat canvW / 2

centerY : CU
centerY = toFloat canvH / 2

worldToCU : Model -> WorldU -> WorldU -> (CU, CU)
worldToCU m wx wy = (centerX + (wx - m.x), centerY + (wy - m.y))

worldToCUPoint : Model -> Point -> (CU, CU)
worldToCUPoint m (pointx, pointy) = worldToCU m pointx pointy

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
    background m ++ bushesView m ++ axesView m ++ playerView ++ lobsView m

background : Model -> List Renderable
background _ = [ shapes [ fill Color.lightGray ]
                        [ rect ( 0, 0 ) (toFloat canvW) (toFloat canvH) ] ]

bushesView : Model -> List Renderable
bushesView m = List.map (bushView m) bushes

bushView : Model -> ( Float, Float ) -> Renderable
bushView m ( bx, by ) = shapes [ fill Color.green ] [ rect ( worldToCU m bx by ) 20 20 ]

playerView : List Renderable
playerView = [ shapes [ fill Color.blue ]
                      [ rect ( centerX - playerSize / 2, centerY - playerSize / 2 ) playerSize playerSize ] ]

playerCU : Model -> Point
playerCU m = worldToCU m m.x m.y

lobsView : Model -> List Renderable
lobsView m = [ shapes [] (List.map (drawOneLob m) m.lobs) ]

drawOneLob : Model -> Lob -> Shape
drawOneLob m lob = drawLine (worldToCUPoint m lob.source) (worldToCUPoint m lob.target)

currentLob : Model -> Lob
currentLob m = { source = (m.x, m.y), target = (50, 200) }

recordLob : Model -> Model
recordLob m =
    { m | lobs = currentLob m :: m.lobs }

drawLine : Point -> Point -> Shape    -- source:  joakin elm-canvas TiledLines
drawLine start end = path start [ lineTo end ]

axesView : Model -> List Renderable
axesView m = [ shapes [] [ drawLine (worldToCU m -2000 0) (worldToCU m 2000 0)
                         , drawLine (worldToCU m 0 -2000) (worldToCU m 0 2000) ] ]

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