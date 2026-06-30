module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Renderable, Shape, lineTo, path, rect, shapes)
import Canvas.Settings exposing (fill, stroke)
import Color
import Html exposing (Html, button, div, form, i, input, p, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode


type alias Model =
    { player : WPoint
    , dirx : Float
    , diry : Float
    , lobs : List BadLob
    , panCenter : Maybe WPoint
    , isMouseDown : Bool
    , showMessages : Bool
    , zoom : Float
    , input : String
    , submittedText : String
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


type alias Azimuth =
    { fillthisfromdart: Float
    }


type alias Power =
    { fillthisfromdart: Float
    }    


type alias Lob =
    { source : WPoint
    , azimuth : Azimuth
    , power : Power
    }


type alias BadLob = 
    { start: WPoint
    , end: WPoint }


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
    | InputChanged String
    | Submit


initialModel : Model
initialModel =
    { player = { x = 100, y = 100 }
    , dirx = 0
    , diry = 0
    , lobs = [ { start = {x = 0, y = 0}, end = {x = 100, y = 200} }
               , { start = {x = 100, y = 0}, end = {x = -100, y = 200} }]
    , panCenter = Nothing
    , isMouseDown = False
    , showMessages = False
    , zoom = 1.0
    , input = ""
    , submittedText = ""
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

        InputChanged newText ->
            ( updateInput newText m, Cmd.none )

        Submit ->
            ( submitInput m, Cmd.none )


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
            , inputForm m
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
        ++ lobsView m.zoom center m.lobs


lobsView : Float -> WPoint -> List BadLob -> List Renderable
lobsView zoom center lobs = 
    let
        drawOneLob : BadLob -> Shape
        drawOneLob lob = oCZLine zoom center lob.start lob.end
    in
        [ shapes [ stroke Color.orange ]
                 ( List.map drawOneLob lobs ) ]




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
    button [ Html.Attributes.title "Merge", class "game-btn" ]
           [ i [ class iconName ] [] ]


zoomInButton : Html Msg
zoomInButton = 
    button [ Html.Attributes.title "Zoom in", class "game-btn", onClick ZoomIn ]
        [ i [ class "fa-solid fa-magnifying-glass-plus fa-2x" ] []
        ]


zoomOutButton : Html Msg
zoomOutButton = 
    button [ Html.Attributes.title "Zoom out", class "game-btn", onClick ZoomOut ]
        [ i [ class "fa-solid fa-magnifying-glass-minus fa-2x" ] []
        ]


messageButton : Html Msg
messageButton =
    button [ Html.Attributes.title "Messages", class "game-btn", onClick ToggleMessages ]
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
    button [ Html.Attributes.title "Back", class "backbtn game-btn", onClick ToggleMessages ]
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
oCZRect zoom center wp w h =
    let
        canvpoint : CPoint
        canvpoint = worldToCanvas zoom center wp
        xcentered = canvpoint.cx - w/2
        ycentered = canvpoint.cy - h/2
    in
    rect (xcentered, ycentered) w h


drawLineRaw : CPoint -> CPoint -> Shape
drawLineRaw start end =
    path (start.cx, start.cy) [ lineTo (end.cx, end.cy) ]

-- offset centered zoomed line
oCZLine : Float -> WPoint -> WPoint -> WPoint -> Shape
oCZLine zoom center begin end =
    let
        canvb = worldToCanvas zoom center begin
        canve = worldToCanvas zoom center end
    in
    drawLineRaw canvb canve


updateInput : String -> Model -> Model
updateInput newText m =
    { m | input = newText }


submitInput : Model -> Model
submitInput m =
    { m
        | submittedText = m.input
        , input = ""
    }


inputForm : Model -> Html Msg
inputForm m =
    form
        [ class "input-form"
        , onSubmit Submit
        ]
        [ input
            [ class "input-form-textfield"
            , type_ "text"
            , placeholder "Enter coordinates..."
            , value m.input
            , onInput InputChanged
            ]
            []
        , button
            [ Html.Attributes.title "Submit", class "game-btn"
            , type_ "submit"
            ]
            [ text "Submit" ]
        ]


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