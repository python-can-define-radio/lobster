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


worldToCanvas : WPoint -> WPoint -> CPoint
worldToCanvas center p =
    { cx = p.x - center.x + toFloat canvW / 2
    , cy = center.y - p.y + toFloat canvH / 2
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
            ( handleMouseMove dx dy model, Cmd.none )

        ToggleMessages ->
            ( { model | showMessages = not model.showMessages }, Cmd.none )

        Recenter ->
            ( {model | panCenter = Nothing }, Cmd.none)


handleMouseMove : Float -> Float -> Model -> Model
handleMouseMove dx dy m =
    if m.isMouseDown then
        { m | panCenter = Just (computeNewPanCenter dx dy m) }

    else
        m


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
    lifeBckgrd
        ++ bushesView m m.player
        ++ avatarView m m.player


tabletScene : Model -> List Renderable
tabletScene m =
    tabletBckgrd
        ++ bushesView m (cameraCenter m)
        ++ avatarView m (cameraCenter m)


screenCenter : CPoint
screenCenter =
    { cx = toFloat canvW / 2
    , cy = toFloat canvH / 2
    }


avatarView : Model -> WPoint -> List Renderable
avatarView m center =
    [ shapes [ fill Color.blue ]
        [ oCRect center m.player 30 30 ]
    ]


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
        , iconButton "fa-solid fa-magnifying-glass-plus fa-2x"
        , iconButton "fa-solid fa-magnifying-glass-minus fa-2x"
        , messageButton
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


iconButton : String -> Html Msg
iconButton iconName =
    button [ class "game-btn" ]
           [ i [ class iconName ] [] ]


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
    List.map (bushView center) bushes


bushView : WPoint -> WPoint -> Renderable
bushView center bushLoc =
    shapes [ fill Color.green ]
           [ oCRect center bushLoc 20 20 ]


-- offset centered rectangle
oCRect : WPoint -> WPoint -> Float -> Float -> Shape
oCRect center p w h =
    let
        cp =
            worldToCanvas center p
    in
    rect (cp.cx - w/2, cp.cy - h/2) w h


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