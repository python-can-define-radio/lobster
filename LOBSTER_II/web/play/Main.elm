module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Point, Renderable, Shape, lineTo, path, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Color
import Html exposing (Html, div, text)
import Json.Decode as Decode
import Html.Attributes exposing (class)

type alias Model = 
    { player : WPoint
    , vx : Float
    , vy : Float
    , lobs : List Lob
    , panCenter : Maybe WPoint
    , isMouseDown : Bool
    }

-- World units Point
type alias WPoint = { x : Float, y : Float }

-- Canvas units Point
type alias CPoint = { cx : Float, cy : Float }

type alias Lob = { source : Point, target : Point }

type Msg
    = KeyDown String
    | KeyUp String
    | Tick Float
    | MouseDown (Float, Float)
    | MouseMove (Float, Float)
    | MouseUp

initialModel : Model
initialModel = 
    { player = { x = 100, y = 100 }
    , vx = 0
    , vy = 0
    , lobs = []
    , panCenter = Nothing
    , isMouseDown = False
    }

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
move dt m = { m | player = 
                { x = m.player.x + m.vx * distperMs * dt, y = m.player.y + m.vy * distperMs * dt }
            }

timestep : Float -> Model -> Model
timestep dt m = move dt m
    -- recordLob (move dt m)

worldToCanvas : WPoint -> WPoint -> CPoint
worldToCanvas center p =
    { cx = p.x - center.x + toFloat canvW / 2
    , cy = center.y - p.y + toFloat canvH / 2
    }

posText : Model -> String
posText m =
    "x: " ++ String.fromInt (round m.player.x)
    ++ ", y: " ++ String.fromInt (round m.player.y)
    
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown k -> ( handleDown k model, Cmd.none )
        KeyUp k -> ( handleUp k model, Cmd.none )
        Tick dt -> ( timestep dt model, Cmd.none )
        MouseDown _ -> ( { model | isMouseDown = True }, Cmd.none )
        MouseUp -> ( { model | isMouseDown = False }, Cmd.none )
        MouseMove (dx, dy) -> handleMouseMove dx dy model

handleMouseMove : Float -> Float -> Model -> ( Model, Cmd Msg)
handleMouseMove dx dy m =
    if m.isMouseDown
    then ( { m | panCenter = computeNewPanCenter dx dy m }, Cmd.none )
    else ( m, Cmd.none )

computeNewPanCenter : Float -> Float -> Model -> Maybe WPoint
computeNewPanCenter dx dy m =
    case m.panCenter of 
        Just x -> Just { x = 0, y = 0 }
        Nothing -> Just { x = 0, y = 0 }
todoForNewPanCenter = "possible idea:  x + dx, y + dy"

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
        "w" -> setVy m 1
        "s" -> setVy m -1
        "a" -> setVx m -1
        "d" -> setVx m 1
        _ -> m

view : Model -> Html Msg
view m =
    div []
        [ div [class "two-canvasses"]
            [ lifeView m
            , tabletView m
            ]
        ]

lifeView : Model -> Html Msg
lifeView m =
    div [ class "life"] [
        Canvas.toHtml ( canvW, canvH ) [] (lifeScene m)
    ]

tabletView : Model -> Html Msg
tabletView m =
    div [ class "hudwrap" ] [
        div [ class "hud" ]
            [ Canvas.toHtml ( canvW, canvH ) [] (tabletScene m)
            , div [class "player-pos"] [text (posText m)] ]
        ]

lifeScene : Model -> List Renderable
lifeScene m =
    let c = m.player in 
    lifeBckgrd ++ bushesView m c ++ playerView

tabletScene : Model -> List Renderable
tabletScene m =
    let 
        c = m.player
    in 
        tabletBckgrd ++ bushesView m c ++ playerView
todoForTabletScene = "++ axesView c ++ lobsView m"


lifeBckgrd : List Renderable
lifeBckgrd =
    [ shapes [ fill (Color.rgb 0.8 1 0.8) ]
        [ rect ( 0, 0 ) (toFloat canvW) (toFloat canvH ) ] ]

tabletBckgrd : List Renderable
tabletBckgrd =
    [ shapes [ fill (Color.rgb 0.2 0.2 0.2) ]
        [ rect ( 0, 0 ) (toFloat canvW) (toFloat canvH ) ] ]

bushes : List WPoint
bushes = [ { x = 200, y = 300 }, { x = 400, y = 150 }, { x = 0, y = 0}, { x = -100, y = -100}, { x = 100, y = 100}, { x = -100, y = 100}, { x = 100, y = -100} ]

bushesView : Model -> WPoint -> List Renderable
bushesView m center = List.map (bushView m center) bushes

bushView : Model -> WPoint -> WPoint -> Renderable
bushView m center bushLoc =
    shapes [ fill Color.green ]
        [ oRect center bushLoc 20 20 ]

oRect : WPoint -> WPoint -> Float -> Float -> Shape
oRect center p w h =
    let cp = worldToCanvas center p
    in rect (cp.cx, cp.cy) w h

centeredSq : CPoint -> Float -> Shape
centeredSq p size = 
    let
        xShifted = p.cx - size / 2
        yShifted = p.cy - size / 2 in
        rect (xShifted, yShifted) size size

playerView : List Renderable
playerView =
    let
        avSize = 30
        halfCanv = { cx = toFloat canvW / 2, cy = toFloat canvH / 2 }
    in
        [ shapes [ fill Color.blue ]
            [ centeredSq halfCanv avSize ] ]

stuffToBringBackAxesEtc = """
-- axesView : Point -> List Renderable
-- axesView c =
   --  [ shapes [ stroke Color.white ]
      --   [ drawLine (worldToCU c -2000 0) (worldToCU c 2000 0)
       -- , drawLine (worldToCU c 0 -2000) (worldToCU c 0 2000) ] ]

-- lobsView : Model -> List Renderable
-- lobsView m =
    -- [ shapes [ stroke Color.orange ]
       --  (List.map (drawOneLob m) m.lobs) ]

-- drawOneLob : Model -> Lob -> Shape
-- drawOneLob m lob =
    -- drawLine (worldToCUPoint m lob.source) (worldToCUPoint m lob.target)

-- currentLob : Model -> Lob
-- currentLob m = { source = ( m.x, m.y ), target = ( 50, 200 ) }

-- recordLob : Model -> Model
-- recordLob m = { m | lobs = currentLob m :: m.lobs }
"""

drawLine : Point -> Point -> Shape
drawLine start end = path start [ lineTo end ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
        , Browser.Events.onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
        , Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onMouseDown (Decode.map MouseDown decodeMouse)
        , Browser.Events.onMouseMove (Decode.map MouseMove decodeMouse)
        , Browser.Events.onMouseUp (Decode.succeed MouseUp)
        ]

decodeMouse : Decode.Decoder (Float, Float)
decodeMouse = 
    Decode.map2 Tuple.pair 
        (Decode.field "clientX" Decode.float) 
        (Decode.field "clientY" Decode.float)

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }