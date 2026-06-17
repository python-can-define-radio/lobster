module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Point, Renderable, Shape, lineTo, path, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Color
import Html exposing (Html, button, div, i, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode


type alias Model =
    { player : WPoint
    , vx : Float
    , vy : Float
    , lobs : List Lob
    , panCenter : Maybe WPoint
    , isMouseDown : Bool
    }


type alias WPoint =
    { x : Float
    , y : Float
    }


type alias CPoint =
    { cx : Float
    , cy : Float
    }


type alias Lob =
    { source : Point
    , target : Point
    }


type Msg
    = KeyDown String
    | KeyUp String
    | Tick Float
    | MouseDown
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
setVx m v =
    { m | vx = v }


setVy : Model -> Float -> Model
setVy m v =
    { m | vy = v }


move : Float -> Model -> Model
move dt m =
    { m
        | player =
            { x = m.player.x + m.vx * distperMs * dt
            , y = m.player.y + m.vy * distperMs * dt
            }
    }


timestep : Float -> Model -> Model
timestep dt m =
    move dt m


cameraCenter : Model -> WPoint
cameraCenter m =
    case m.panCenter of
        Just center ->
            center

        Nothing ->
            m.player


worldToCanvas : WPoint -> WPoint -> CPoint
worldToCanvas center p =
    { cx = p.x - center.x + toFloat canvW / 2
    , cy = center.y - p.y + toFloat canvH / 2
    }


posText : Model -> String
posText m =
    "x: "
        ++ String.fromInt (round m.player.x)
        ++ ", y: "
        ++ String.fromInt (round m.player.y)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown k ->
            ( handleDown k model, Cmd.none )

        KeyUp k ->
            ( handleUp k model, Cmd.none )

        Tick dt ->
            ( timestep dt model, Cmd.none )

        MouseDown ->
            ( { model | isMouseDown = True }, Cmd.none )

        MouseUp ->
            ( { model | isMouseDown = False }, Cmd.none )

        MouseMove (dx, dy) ->
            handleMouseMove dx dy model


handleMouseMove : Float -> Float -> Model -> ( Model, Cmd Msg )
handleMouseMove dx dy m =
    if m.isMouseDown then
        ( { m | panCenter = Just (computeNewPanCenter dx dy m) }
        , Cmd.none
        )

    else
        ( m, Cmd.none )


computeNewPanCenter : Float -> Float -> Model -> WPoint
computeNewPanCenter dx dy m =
    let
        current =
            cameraCenter m
    in
    { x = current.x - dx
    , y = current.y + dy
    }


handleUp : String -> Model -> Model
handleUp k m =
    case String.toLower k of
        "w" ->
            setVy m 0

        "s" ->
            setVy m 0

        "a" ->
            setVx m 0

        "d" ->
            setVx m 0

        _ ->
            m


handleDown : String -> Model -> Model
handleDown k m =
    case String.toLower k of
        "w" ->
            setVy m 1

        "s" ->
            setVy m -1

        "a" ->
            setVx m -1

        "d" ->
            setVx m 1

        _ ->
            m


view : Model -> Html Msg
view m =
    div []
        [ div [ class "two-canvasses" ]
            [ lifeView m
            , tabletView m
            ]
        ]


lifeView : Model -> Html Msg
lifeView m =
    div [ class "life" ]
        [ Canvas.toHtml ( canvW, canvH ) [] (lifeScene m)
        ]


tabletView : Model -> Html Msg
tabletView m =
    div [ class "hudwrap" ]
        [ div [ class "tablet-area" ]
            [ div [ class "hud" ]
                [ Canvas.toHtml ( canvW, canvH ) [] (tabletScene m)
                , div [ class "player-pos" ] [ text (posText m) ]
                , tabletButtons
                ]
            ]
        ]


lifeScene : Model -> List Renderable
lifeScene m =
    lifeBckgrd
        ++ bushesView m m.player
        ++ avatarView


tabletScene : Model -> List Renderable
tabletScene m =
    tabletBckgrd
        ++ bushesView m (cameraCenter m)
        ++ avatarView


screenCenter : CPoint
screenCenter =
    { cx = toFloat canvW / 2
    , cy = toFloat canvH / 2
    }


avatarView : List Renderable
avatarView =
    [ shapes [ fill Color.blue ]
        [ centeredSq screenCenter 30 ]
    ]


lifeBckgrd : List Renderable
lifeBckgrd =
    [ shapes [ fill (Color.rgb 0.8 1 0.8) ]
        [ rect (0, 0) (toFloat canvW) (toFloat canvH) ]
    ]


tabletBckgrd : List Renderable
tabletBckgrd =
    [ shapes [ fill (Color.rgb 0.2 0.2 0.2) ]
        [ rect (0, 0) (toFloat canvW) (toFloat canvH) ]
    ]


tabletButtons : Html Msg
tabletButtons =
    div [ class "tablet-buttons" ]
        [ iconButton "fa-solid fa-object-group fa-2x"
        , iconButton "fa-solid fa-magnifying-glass-plus fa-2x"
        , iconButton "fa-solid fa-magnifying-glass-minus fa-2x"
        , iconButton "fa-solid fa-envelope fa-2x"
        ]


iconButton : String -> Html Msg
iconButton iconName =
    button [ class "game-btn" ]
        [ i [ class iconName ] []
        ]

        
bushes : List WPoint
bushes =
    [ { x = 200, y = 300 }
    , { x = 400, y = 150 }
    , { x = 0, y = 0 }
    , { x = -100, y = -100 }
    , { x = 100, y = 100 }
    , { x = -100, y = 100 }
    , { x = 100, y = -100 }
    ]


bushesView : Model -> WPoint -> List Renderable
bushesView m center =
    List.map (bushView m center) bushes


bushView : Model -> WPoint -> WPoint -> Renderable
bushView m center bushLoc =
    shapes [ fill Color.green ]
        [ oRect center bushLoc 20 20 ]


oRect : WPoint -> WPoint -> Float -> Float -> Shape
oRect center p w h =
    let
        cp =
            worldToCanvas center p
    in
    rect (cp.cx, cp.cy) w h


centeredSq : CPoint -> Float -> Shape
centeredSq p size =
    rect
        ( p.cx - size / 2
        , p.cy - size / 2
        )
        size
        size


drawLine : Point -> Point -> Shape
drawLine start end =
    path start [ lineTo end ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Decode.map KeyDown (Decode.field "key" Decode.string))
        , Browser.Events.onKeyUp
            (Decode.map KeyUp (Decode.field "key" Decode.string))
        , Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onMouseDown
            (Decode.succeed MouseDown)
        , Browser.Events.onMouseMove
            (Decode.map MouseMove decodeMouseMovement)
        , Browser.Events.onMouseUp
            (Decode.succeed MouseUp)
        ]


decodeMouseMovement : Decode.Decoder (Float, Float)
decodeMouseMovement =
    Decode.map2 Tuple.pair
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }