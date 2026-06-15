module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Point, Renderable, Shape, lineTo, path, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Color
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Html.Attributes exposing (class)

type alias Model =
    { x : Float, y : Float, vx : Float, vy : Float, lobs : List Lob }

type alias Lob =
    { source : Point, target : Point }

type alias WorldU = Float
type alias Vel = Float
type alias CU = Float
type alias Ms = Float
type alias DistPerMs = Float

type Msg
    = KeyDown String
    | KeyUp String
    | Tick Float

initialModel : Model
initialModel = { x = 100, y = 100, vx = 0, vy = 0, lobs = [] }

speed : DistPerMs
speed = 0.2

playerSize : CU
playerSize = 50

canvW : Int
canvW = 600

canvH : Int
canvH = 400

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

worldToCU : Model -> WorldU -> WorldU -> ( CU, CU )
worldToCU m wx wy = ( centerX + (wx - m.x), centerY + (wy - m.y) )

worldToCUPoint : Model -> Point -> ( CU, CU )
worldToCUPoint m ( px, py ) = worldToCU m px py

playerCU : Model -> Point
playerCU m = worldToCU m m.x m.y

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
        [ div canvWrapperStyles
            [ worldView m
            , tabletView m
            ]
        , p [] [ text (posText m) ]
        ]

tabletView : Model -> Html Msg
tabletView m =
    div [ class "hudwrap" ] [
        div [ class "hud" ]
            [ Canvas.toHtml ( canvW, canvH ) [] (tabletScene m) ]
        ]

worldView : Model -> Html Msg
worldView m =
    div [ class "life"] [
        Canvas.toHtml ( canvW, canvH ) worldStyles (worldScene m)
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

playerView : List Renderable
playerView =
    [ shapes [ fill Color.blue ]
        [ rect ( centerX - playerSize / 2, centerY - playerSize / 2 )
            playerSize playerSize ] ]

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

-- styles
canvWrapperStyles : List (Html.Attribute Msg)
canvWrapperStyles =
    [ style "display" "flex"
    , style "gap" "24px"
    , style "align-items" "flex-start"
    ]

-- tabletDeviceStyles : List (Html.Attribute Msg)
-- tabletDeviceStyles =
--     [ style "background-color" "#2f3136"
--     , style "padding" "16px"
--     , style "border-radius" "24px"
--     , style "box-shadow" "0px 8px 20px rgba(0,0,0,0.45)"
--     , style "position" "relative"
--     ]

-- tabletScreenStyles : List (Html.Attribute Msg)
-- tabletScreenStyles = []
    -- [ style "background-color" "#1a1a1a"
    -- , style "padding" "4px"
    -- , style "border-radius" "4px"
    -- , style "overflow" "hidden"
    -- , style "box-shadow" "inset 0px 0px 4px rgba(0,0,0,0.5)"
    -- ]

worldStyles : List (Html.Attribute Msg)
worldStyles =
    [ style "background-color" "#0b0b0b"
    , style "border" "2px solid black"
    , style "display" "block"
    ]

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }