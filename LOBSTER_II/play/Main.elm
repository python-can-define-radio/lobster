module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Point, Renderable, Shape, lineTo, path, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Color
import Html exposing (Html, button, div, i, text, p)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode


type alias Model =
    { player : WPoint
    , dirx : Float
    , diry : Float
    , lobs : List Lob
    , panCenter : Maybe WPoint
    , isMouseDown : Bool
    , showMessages : Bool
    , zoom : Float
    }


type alias WPoint =
    { x : Float
    , y : Float
    }


type alias CPoint =
    { cx : Float
    , cy : Float
    }

type alias CDiff =
    { cdx : Float
    , cdy : Float
    }

type alias Lob =
    { source : WPoint
    , target : WPoint
    }


type Msg
    = KeyDown String
    | KeyUp String
    | Tick Float
    | MouseDown
    | MouseMove (Float, Float)
    | MouseUp
    | ToggleMessages
    | ZoomIn
    | ZoomOut
    | Recenter


initialModel : Model
initialModel =
    { player = { x = 100, y = 100 }
    , dirx = 0
    , diry = 0
    , lobs = []
    , panCenter = Nothing
    , isMouseDown = False
    , showMessages = False
    , zoom = 1.0
    }


distperMs : Float
distperMs = 0.2


canvW : Int
canvW = 600


canvH : Int
canvH = 400


move : Float -> Model -> Model
move dt m =
    { m
        | player =
            { x = m.player.x + m.dirx * distperMs * dt
            , y = m.player.y + m.diry * distperMs * dt
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


worldToCanvas : Float -> WPoint -> WPoint -> CPoint
worldToCanvas zoom center p =
    { cx = zoom * (p.x - center.x) + toFloat canvW / 2
    , cy = zoom * (center.y - p.y) + toFloat canvH / 2
    }


posText : Model -> Html Msg
posText m =
    div [ class "player-pos" ]
        [ text (
            "x: "
            ++ String.fromInt (round m.player.x)
            ++ ", y: "
            ++ String.fromInt (round m.player.y)
        ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        KeyDown k ->
            ( handleDown k m, Cmd.none )

        KeyUp k ->
            ( handleUp k m, Cmd.none )

        Tick dt ->
            ( timestep dt m, Cmd.none )

        MouseDown ->
            ( { m | isMouseDown = True }, Cmd.none )

        MouseUp ->
            ( { m | isMouseDown = False }, Cmd.none )

        MouseMove (dx, dy) ->
            ( handleMouseMove {cdx = dx, cdy = dy} m, Cmd.none )

        ToggleMessages ->
            ( { m | showMessages = not m.showMessages }, Cmd.none )

        ZoomIn ->
            ( { m | zoom = m.zoom * 2 }, Cmd.none)

        ZoomOut ->
            ( { m | zoom = m.zoom / 2 }, Cmd.none)

        Recenter ->
            ( { m | panCenter = Nothing }, Cmd.none)


handleMouseMove : CDiff -> Model -> Model
handleMouseMove cdiff m =
    if m.isMouseDown then
        { m | panCenter = Just (computeNewPanCenter cdiff m) }

    else
        m


computeNewPanCenter : CDiff -> Model -> WPoint
computeNewPanCenter cdiff m =
    let
        current =
            cameraCenter m
    in
    { x = current.x - cdiff.cdx / m.zoom
    , y = current.y + cdiff.cdy / m.zoom
    }


handleUp : String -> Model -> Model
handleUp k m =
    case String.toLower k of
        "w" ->
            { m | diry = 0 }

        "s" ->
            { m | diry = 0 }

        "a" ->
            { m | dirx = 0 }

        "d" ->
            { m | dirx = 0 }

        _ ->
            m


handleDown : String -> Model -> Model
handleDown k m =
    case String.toLower k of
        "w" ->
            { m | diry = 1 }

        "s" ->
            { m | diry = -1 }

        "a" ->
            { m | dirx = -1 }

        "d" ->
            { m | dirx = 1 }

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
        [ Canvas.toHtml (canvW, canvH) [] (lifeScene m)
        ]


tabletView : Model -> Html Msg
tabletView m =
    div [ class "hudwrap" ]
        [ div [ class "hud" ]
            [ Canvas.toHtml (canvW, canvH) [] (tabletScene m)
            , posText m
            , tabletButtons
            , recenterButton m
            , messagesOverlay m
            ]
        ]
        

recenterButton : Model -> Html Msg
recenterButton m =
    case m.panCenter of
        Nothing -> 
            div [ class "hidden" ] []
        _ ->
            button [ class "recenter-btn game-btn", onClick Recenter ] 
                   [ text "Re-center" ]


lifeScene : Model -> List Renderable
lifeScene m =
    let
        center = m.player
        zoom = 1
    in
        lifeBckgrd
            ++ bushesView zoom center
            ++ avatarView zoom center m.player


tabletScene : Model -> List Renderable
tabletScene m =
    let center = cameraCenter m in
    tabletBckgrd
        ++ bushesView m.zoom center
        ++ avatarView m.zoom center m.player


screenCenter : CPoint
screenCenter =
    { cx = toFloat canvW / 2
    , cy = toFloat canvH / 2
    }


avatarView : Float -> WPoint -> WPoint -> List Renderable
avatarView zoom center player =
    let
        size = 30 * zoom 
        clsize = clamp 20 99999 size
    in
        [ shapes [ fill Color.blue ]
                 [ oCZRect zoom center player clsize clsize ] ]


lifeBckgrd : List Renderable
lifeBckgrd =
    [ shapes [ fill (Color.rgb 0.8 1 0.8) ]
        [ rect (0, 0) (toFloat canvW) (toFloat canvH) ]
    ]


tabletBckgrd : List Renderable
tabletBckgrd =
    [ shapes [ fill (Color.rgb 0.1 0.1 0.1) ]
        [ rect (0, 0) (toFloat canvW) (toFloat canvH) ]
    ]


tabletButtons : Html Msg
tabletButtons =
    div [ class "tablet-buttons" ]
        [ iconButton "fa-solid fa-object-group fa-2x"
        , zoomInButton 
        , zoomOutButton
        , messageButton
        ]

        
iconButton : String -> Html Msg
iconButton iconName =
    button [ class "game-btn" ]
           [ i [ class iconName ] [] ]


zoomInButton : Html Msg
zoomInButton = 
    button [ class "game-btn", onClick ZoomIn ]
        [ i [ class "fa-solid fa-magnifying-glass-plus fa-2x" ] []
        ]


zoomOutButton : Html Msg
zoomOutButton = 
    button [ class "game-btn", onClick ZoomOut ]
        [ i [ class "fa-solid fa-magnifying-glass-minus fa-2x" ] []
        ]


messageButton : Html Msg
messageButton =
    button [ class "game-btn", onClick ToggleMessages ]
        [ i [ class "fa-solid fa-envelope fa-2x" ] []
        ]


messagesOverlay : Model -> Html Msg
messagesOverlay m =
    if m.showMessages then
        div [ class "overlay" ]
            [ overlayBackButton
            , missionMessage
            ] 

    else
        div [ class "hidden" ] []


overlayBackButton : Html Msg
overlayBackButton =
    button [ class "backbtn game-btn", onClick ToggleMessages ]
           [ i [ class "fa-solid fa-chevron-left fa-2x" ] [] ]


missionMessage :  Html Msg
missionMessage =
    div [ class "mission-message" ]
        [ i [ class "fa-regular fa-user fa-3x"] []
        , p [] [text "Mission Update :: Recover the lost signal beacon near the northern ridge." ] 
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


bushesView : Float -> WPoint -> List Renderable
bushesView zoom center =
    List.map (bushView zoom center) bushes


bushView : Float -> WPoint -> WPoint -> Renderable
bushView zoom center bushLoc =
    let 
        size = 20 * zoom
    in
        shapes [ fill Color.green ]
               [ oCZRect zoom center bushLoc size size ]


-- offset centered zoomed rectangle
oCZRect : Float -> WPoint -> WPoint -> Float -> Float -> Shape
oCZRect zoom center wp wzoom hzoom =
    let
        canvpoint : CPoint
        canvpoint = worldToCanvas zoom center wp
        xcentered = canvpoint.cx - wzoom/2
        ycentered = canvpoint.cy - hzoom/2
    in
    rect (xcentered, ycentered) wzoom hzoom


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