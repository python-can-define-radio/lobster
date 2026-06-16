module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Point, Renderable, Shape, lineTo, path, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Color
import Html exposing (Html, div, text)
import Json.Decode as Decode
import Html.Attributes exposing (class)

type alias Model = { x : Float, y : Float, vx : Float, vy : Float, lobs : List Lob }
type alias Lob = { source : Point, target : Point }

type Msg
    = KeyDown String
    | KeyUp String
    | Tick Float

initialModel : Model
initialModel = { x = 100, y = 100, vx = 0, vy = 0, lobs = [] }

distperMs : Float
distperMs = 0.2

canvW : Int
canvW = 600

canvH : Int
canvH = 400

setVx : Model -> Float -> Model
setVx m v = { m | vx = v }

setVy : Model -> Float -> Model
setVy m v = { m | vy = v }

move : Float -> Model -> Model
move dt m = { m | x = m.x + m.vx * distperMs * dt, y = m.y + m.vy * distperMs * dt }

timestep : Float -> Model -> Model
timestep dt m = recordLob (move dt m)

centerX : Float
centerX = toFloat canvW / 2

centerY : Float
centerY = toFloat canvH / 2

worldToCU : Model -> Float -> Float -> ( Float, Float )
worldToCU m wx wy = ( centerX + (wx - m.x), centerY + (wy - m.y) )

worldToCUPoint : Model -> Point -> ( Float, Float )
worldToCUPoint m ( px, py ) = worldToCU m px py

posText : Model -> String
posText m =
    "x: " ++ String.fromInt (round m.x)
    ++ ", y: " ++ String.fromInt (round m.y)
    
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown k -> ( handleDown k model, Cmd.none )
        KeyUp k -> ( handleUp k model, Cmd.none )
        Tick dt -> ( timestep dt model, Cmd.none )

handleUp : String -> Model -> Model
handleUp k m =
    case String.toLower k of
        "w" -> setVy m 0
        "s" -> setVy m 0
        "a" -> setVx m 0
        "d" -> setVx m 0
        _ -> m

handleDown : String -> Model -> Model
handleDown k m =
    case String.toLower k of
        "w" -> setVy m -1
        "s" -> setVy m 1
        "a" -> setVx m -1
        "d" -> setVx m 1
        _ -> m

view : Model -> Html Msg
view m =
    div []
        [ div [class "two-canvasses"]
            [ worldView m
            , tabletView m
            ]
        ]

worldView : Model -> Html Msg
worldView m =
    div [ class "life"] [
        Canvas.toHtml ( canvW, canvH ) [] (worldScene m)
    ]

tabletView : Model -> Html Msg
tabletView m =
    div [ class "hudwrap" ] [
        div [ class "hud" ]
            [ Canvas.toHtml ( canvW, canvH ) [] (tabletScene m)
            , div [class "player-pos"] [text (posText m)] ]
        ]

worldScene : Model -> List Renderable
worldScene m =
    worldBckgrd m ++ bushesView m ++ playerView

tabletScene : Model -> List Renderable
tabletScene m =
    tabletBckgrd m ++ bushesView m ++ axesView m ++ playerView ++ lobsView m

worldBckgrd : Model -> List Renderable
worldBckgrd _ =
    [ shapes [ fill (Color.rgb 0.8 1 0.8) ]
        [ rect ( 0, 0 ) (toFloat canvW) (toFloat canvH ) ] ]

tabletBckgrd : Model -> List Renderable
tabletBckgrd _ =
    [ shapes [ fill (Color.rgb 0.2 0.2 0.2) ]
        [ rect ( 0, 0 ) (toFloat canvW) (toFloat canvH ) ] ]

bushes : List Point
bushes = [ ( 200, 200 ), ( 400, 150 ), ( 300, 350 ) ]

bushesView : Model -> List Renderable
bushesView m = List.map (bushView m) bushes

bushView : Model -> ( Float, Float ) -> Renderable
bushView m ( bx, by ) =
    shapes [ fill Color.green ]
        [ rect ( worldToCU m bx by ) 20 20 ]

centeredSq : Float -> Float -> Float -> Shape
centeredSq x y size = 
    let
        xShifted = x - size / 2
        yShifted = y - size / 2 in
        rect (xShifted, yShifted) size size

playerView : List Renderable
playerView = let avSize = 30 in
    [ shapes [ fill Color.blue ]
        [ centeredSq centerX centerY avSize ] ]

axesView : Model -> List Renderable
axesView m =
    [ shapes [ stroke Color.white ]
        [ drawLine (worldToCU m -2000 0) (worldToCU m 2000 0)
        , drawLine (worldToCU m 0 -2000) (worldToCU m 0 2000) ] ]

lobsView : Model -> List Renderable
lobsView m =
    [ shapes [ stroke Color.orange ]
        (List.map (drawOneLob m) m.lobs) ]

drawOneLob : Model -> Lob -> Shape
drawOneLob m lob =
    drawLine (worldToCUPoint m lob.source) (worldToCUPoint m lob.target)

currentLob : Model -> Lob
currentLob m = { source = ( m.x, m.y ), target = ( 50, 200 ) }

recordLob : Model -> Model
recordLob m = { m | lobs = currentLob m :: m.lobs }

drawLine : Point -> Point -> Shape
drawLine start end = path start [ lineTo end ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Decode.map KeyDown (Decode.field "key" Decode.string))
        , Browser.Events.onKeyUp
            (Decode.map KeyUp (Decode.field "key" Decode.string))
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