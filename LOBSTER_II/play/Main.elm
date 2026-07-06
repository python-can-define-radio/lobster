module Main exposing (main)

import Browser
import Browser.Events
import Canvas exposing (Renderable, Shape, lineTo, path, rect, shapes, clear)
import Canvas.Settings exposing (fill, stroke)
import Color
import Canvas.Texture exposing (Texture)
import Canvas
-- import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (transform, scale)
-- import Canvas.Settings.Text exposing (..)
import Canvas.Texture as Texture
import Html exposing (Html, button, div, form, i, input, p, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode
import Random


type alias Model =
    { player : WPoint
    , dirx : Float
    , diry : Float
    , lobs : List Lob
    , transmitter : WPoint
    , panCenter : Maybe WPoint
    , isMouseDown : Bool
    , showMessages : Bool
    , zoom : Float
    , input : String
    , submittedText : String
    , isGatheringLobs : Bool
    , time : Float
    , playerTextures : Maybe PlayerTextures 
    }


type alias PlayerTextures =
    { standby : Texture
    , lff : Texture
    , rff : Texture
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
    { sinresult : Float
    , cosresult : Float
    }


type alias Power =
    { mW : Float
    }


type alias Lob =
    { source : WPoint
    , azimuth : Azimuth
    , power : Power
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
    | ClearLobs
    | InputChanged String
    | Submit
    | ToggleGatherLobs
    | GotLobNoise Float
    | TextureAvSheetLoaded (Maybe Texture)


initialModel : Model
initialModel =
    let
        tx =
            { x = 70220, y = 41000 }
    in
    { player = { x = 70100, y = 40100 }
    , dirx = 0
    , diry = 0
    , lobs = []
    , transmitter = tx
    , panCenter = Nothing
    , isMouseDown = False
    , showMessages = False
    , zoom = 1.0
    , input = ""
    , submittedText = ""
    , isGatheringLobs = True
    , time = 0
    , playerTextures = Nothing
    }


distperMs : Float
distperMs = 0.2


canvW : number
canvW = 600


canvH : number
canvH = 400


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "../assets/avatar_sheet.png" TextureAvSheetLoaded
    ]
    
move : Float -> Model -> Model
move dt m =
    { m
        | player =
            { x = m.player.x + m.dirx * distperMs * dt
            , y = m.player.y + m.diry * distperMs * dt
            }
    }


addCurrentLobWithNoise : Float -> Model -> Model
addCurrentLobWithNoise noise m =
    { m
        | lobs =
            currentLobWithNoise noise m :: m.lobs
    }


currentLobWithNoise : Float -> Model -> Lob
currentLobWithNoise noise m =
    let
        base =
            azimuthFromPositions m.player m.transmitter

        cosA =
            cos noise

        sinA =
            sin noise

        rotated =
            { cosresult =
                base.cosresult * cosA - base.sinresult * sinA
            , sinresult =
                base.cosresult * sinA + base.sinresult * cosA
            }

        dx =
            m.player.x - m.transmitter.x

        dy =
            m.player.y - m.transmitter.y

        dist =
            sqrt (dx * dx + dy * dy)

        basePower =
            100.0

        powerLinear =
            basePower / (1 + (dist * dist))

        power =
            if powerLinear <= 0 then
                -120
            else
                10 * logBase 10 powerLinear
    in
    { source = m.player
    , azimuth = rotated
    , power = { mW = power }
    }


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
            ( move dt m, lobNoise m )

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

        ClearLobs ->
            ( { m | lobs = [] }, Cmd.none )

        ToggleGatherLobs ->
            ( { m | isGatheringLobs = not m.isGatheringLobs }, Cmd.none )

        GotLobNoise angle ->
            ( addCurrentLobWithNoise angle m, Cmd.none )

        InputChanged newText ->
            ( updateInput newText m, Cmd.none )

        Submit ->
            ( submitInput m, Cmd.none )

        TextureAvSheetLoaded Nothing ->
            ( m, Cmd.none )

        TextureAvSheetLoaded (Just avSheet) ->
            ( { m | playerTextures = Just (texturesFromAvSheet avSheet)}
            , Cmd.none
            )

texturesFromAvSheet : Texture -> PlayerTextures
texturesFromAvSheet avSheet = 
    let
        cell = 256

        sprite x y =
            Texture.sprite
                { x = x * cell
                , y = y * cell
                , width = cell
                , height = cell
                }
                avSheet
    in
        { standby = sprite 1 3 
        , lff = sprite 0 3 
        , rff = sprite 2 3
        }




lobNoise : Model -> Cmd Msg
lobNoise m =
    if m.isGatheringLobs
    then Random.generate GotLobNoise (Random.float -0.05 0.05)
    else Cmd.none


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
            { m | diry = 10 }

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
    let
        canvasSettings =
            { width = canvW
            , height = canvH
            , textures = textures
            }
    in
        div [ class "life" ]
            [ Canvas.toHtmlWith canvasSettings [] ( lifeScene m )
            ]


tabletView : Model -> Html Msg
tabletView m =
    div [ class "hudwrap" ]
        [ div [ class "hud" ]
            [ Canvas.toHtml (canvW, canvH)
                [ class "tablet-canvas" ]
                (tabletScene m)
            , Canvas.toHtml (canvW, canvH)
                [ class "lobs-canvas" ]
                (lobsScene m)
            , posText m
            , tabletButtons m
            , clearLobsButton
            , gatherLobsButton m
            , recenterButton m
            , messagesOverlay m
            , lobPowerView m
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
        center =
            m.player

        zoom =
            1
    in
    lifeBckgrd
        ++ bushesView zoom center
        ++ transmitterView zoom center m.transmitter
        ++ avatarView zoom center m.player
        ++ playerRend m


tabletScene : Model -> List Renderable
tabletScene m =
    let
        center =
            cameraCenter m
    in
    tabletBckgrd
        ++ bushesView m.zoom center
        ++ transmitterView m.zoom center m.transmitter
        ++ avatarView m.zoom center m.player


lobsScene : Model -> List Renderable
lobsScene m =
    let
        center =
            cameraCenter m
    in
        ( clear ( 0, 0 ) (toFloat canvW) (toFloat canvH) )
            :: lobsView m.zoom center m.lobs





lobsView : Float -> WPoint -> List Lob -> List Renderable
lobsView zoom center lobs =
    let
        drawOneLob : Lob -> Shape
        drawOneLob lob =
            oCZLine zoom center lob.source (lobEndpoint lob)
    in
    [ shapes [ stroke Color.orange ]
        (List.map drawOneLob lobs)
    ]


lobLength : Float
lobLength =
    50000


lobEndpoint : Lob -> WPoint
lobEndpoint lob =
    { x = lob.source.x + lob.azimuth.cosresult * lobLength
    , y = lob.source.y + lob.azimuth.sinresult * lobLength
    }


azimuthFromPositions : WPoint -> WPoint -> Azimuth
azimuthFromPositions receiver transmitter =
    let
        xd =
            transmitter.x - receiver.x

        yd =
            transmitter.y - receiver.y

        dist =
            sqrt (xd * xd + yd * yd)
    in
    { sinresult = yd / dist
    , cosresult = xd / dist
    }


transmitterView : Float -> WPoint -> WPoint -> List Renderable
transmitterView zoom center tx =
    let
        size =
            20 * zoom
    in
    [ shapes [ fill Color.red ]
        [ oCZRect zoom center tx size size ]
    ]



playerRend : Model -> List Renderable
playerRend m =
    case m.playerTextures of
        Just avSheet ->
            [ walkingAnimation m.time avSheet ]

        Nothing ->
            []


walkingAnimation : Float -> PlayerTextures -> Renderable
walkingAnimation time playerTextures =
            let
                thirdsofsec : Int
                thirdsofsec = time |> round |> remainderBy 1000

                t : Texture
                t =
                    if thirdsofsec < 333 then
                        playerTextures.rff

                    else if thirdsofsec < 666 then
                        playerTextures.standby

                    else
                        playerTextures.lff

            in
            zoomedTexture (0.5) 600 400 t


zoomedTexture : Float -> Float -> Float -> Texture -> Renderable
zoomedTexture zoom x y t =
    Canvas.texture [ transform [ scale zoom zoom ] ] ( x, y ) t

    
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


tabletButtons : Model -> Html Msg
tabletButtons m =
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


gatherLobsButton : Model -> Html Msg
gatherLobsButton m =
    let
        isOn =
            m.isGatheringLobs
    in
    button
        [ class "lob-btn game-btn"
        , onClick ToggleGatherLobs
        , Html.Attributes.title "Toggle LOB gathering"
        ]
        [ i
            [ class
                (if isOn then
                    "fa-solid fa-pause fa-2x col-red"
                 else
                    "fa-solid fa-play fa-2x")
            ]
            []
        ]


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


clearLobsButton : Html Msg
clearLobsButton =
    button
        [ Html.Attributes.title "Clear lobs", class "clear-btn game-btn"
        , onClick ClearLobs
        ]
        [ text "Clear LOBs" ]


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
    [ { x = 70200, y = 40300 }
    , { x = 70400, y = 40150 }
    , { x = 70000, y = 40000 }
    , { x = 69900, y = 39900 }
    , { x = 70100, y = 40100 }
    , { x = 69900, y = 40100 }
    , { x = 70100, y = 39900 }
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


lobPowerView : Model -> Html Msg
lobPowerView m =
    div [ class "lob-power" ]
        [ case List.head m.lobs of
            Just lob ->
                viewLobPower lob

            Nothing ->
                text "Power: ___ dB" ]


format2dp : Float -> String
format2dp value =
    String.fromFloat (toFloat (round (value * 100)) / 100)


viewLobPower : Lob -> Html Msg
viewLobPower lob =
    div []
        [ text ("Power: " ++ format2dp lob.power.mW ++ " dB") ]


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