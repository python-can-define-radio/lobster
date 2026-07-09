module Main exposing (main)

import Browser exposing (element)
import Browser.Events
import Canvas exposing (Renderable, Shape, lineTo, path, rect, shapes, clear)
import Canvas.Settings exposing (fill, stroke)
import Color
import Canvas.Texture exposing (Texture)
import Canvas
import Canvas.Settings.Advanced exposing (transform, scale, translate)
import Canvas.Texture as Texture
import Html exposing (Html, button, div, form, i, input, p, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode
import Random
import Set exposing (Set)


type alias Model =
    { player : WPoint
    , dirx : Float
    , diry : Float
    , lobs : List Lob
    , transmitter : WPoint
    , panCenter : Maybe WPoint
    , isMouseDown : Bool
    , overlayShown : Bool
    , zoom : Float
    , currentText : String
    , submittedText : String
    , isGatheringLobs : Bool
    , time : Float
    , lastFacing : Facing
    , keysDown : Set String
    , playerTextures : Maybe PlayerTextures
    , bushTextures : BushTextures
    , bushes : List Bush
    }


type alias WalkCycle =
    { frame0 : Texture
    , frame1 : Texture
    , frame2 : Texture
    , frame3 : Texture
    }


type alias PlayerTextures =
    { down : WalkCycle
    , up : WalkCycle
    , left : WalkCycle
    , right : WalkCycle
    }


type BushKind
    = Bush1
    | Bush2


type alias Bush =
    { position : WPoint
    , kind : BushKind
    }


type alias BushTextures =
    { bush1 : Maybe Texture
    , bush2 : Maybe Texture
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


type Facing
    = FaceDown
    | FaceUp
    | FaceLeft
    | FaceRight


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
    | ToggleOverlay
    | ZoomIn
    | ZoomOut
    | Recenter
    | ClearLobs
    | TextChanged String
    | Submit
    | ToggleGatherLobs
    | LobNoiseAvailable Float
    | BushesGenerated (List Bush)
    | TextureAvSheetLoaded (Maybe Texture)
    | TextureBush1Loaded (Maybe Texture)
    | TextureBush2Loaded (Maybe Texture)


initialModel : Model
initialModel =
    { player = { x = 70100, y = 40100 }
    , dirx = 0
    , diry = 0
    , lobs = []
    , transmitter = { x = 70220, y = 41000 }
    , panCenter = Nothing
    , isMouseDown = False
    , overlayShown = False
    , zoom = 1.0
    , currentText = ""
    , submittedText = ""
    , isGatheringLobs = True
    , time = 0
    , lastFacing = FaceDown
    , keysDown = Set.empty
    , playerTextures = Nothing
    , bushTextures =
        { bush1 = Nothing
        , bush2 = Nothing
        }
    , bushes = []
    }


distperMs : Float
distperMs = 0.05


canvW : number
canvW = 600


canvH : number
canvH = 400


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "../assets/avatar_sheet2.png" TextureAvSheetLoaded
    , Texture.loadFromImageUrl "../assets/bush_1.png" TextureBush1Loaded
    , Texture.loadFromImageUrl "../assets/bush_2.png" TextureBush2Loaded
    ]
    
move : Float -> Model -> Model
move dt m =
    let
        movement =
            movementVector m

        speed =
            if isRunning m then
                distperMs * 2
            else
                distperMs
    in
    { m
        | player =
            { x = m.player.x + movement.x * speed * dt
            , y = m.player.y + movement.y * speed * dt
            }
    }

makeLob : Float -> Model -> Lob
makeLob noise m =
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


accumulateTime : Float -> Model -> Model
accumulateTime dt m =
    { m | time = m.time + dt }


cameraCenter : Model -> WPoint
cameraCenter m =
    case m.panCenter of
        Just center ->
            center

        Nothing ->
            m.player


type alias PosInfo =
    { zoom : Float
    , center : WPoint
    , point : WPoint
    }


worldToCanvas : PosInfo -> CPoint
worldToCanvas {zoom, center, point} =
    { cx = zoom * (point.x - center.x) + canvW / 2
    , cy = zoom * (center.y - point.y) + canvH / 2
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
            ( m
                |> accumulateTime dt
                |> move dt 
            , lobNoise m
            )

        MouseDown ->
            ( { m | isMouseDown = True }, Cmd.none )

        MouseUp ->
            ( { m | isMouseDown = False }, Cmd.none )

        MouseMove (dx, dy) ->
            ( handleMouseMove {cdx = dx, cdy = dy} m, Cmd.none )

        ToggleOverlay ->
            ( { m | overlayShown = not m.overlayShown }, Cmd.none )

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

        LobNoiseAvailable noise ->
            ( { m | lobs = makeLob noise m :: m.lobs }, Cmd.none )

        TextChanged newText ->
            ( updateText newText m, Cmd.none )

        Submit ->
            ( submitText m, Cmd.none )

        TextureAvSheetLoaded Nothing ->
            ( m, Cmd.none )

        TextureAvSheetLoaded (Just avSheet) ->
            ( { m | playerTextures = Just (texturesFromAvSheet avSheet)}
            , Cmd.none
            )
        
        BushesGenerated bushes ->
            ( { m | bushes = bushes }, Cmd.none )

        TextureBush1Loaded (Just texture) ->
            ( { m
                | bushTextures =
                    { bush1 = Just texture
                    , bush2 = m.bushTextures.bush2
                    }
              }
            , Cmd.none
            )

        TextureBush1Loaded Nothing ->
            ( m, Cmd.none )

        TextureBush2Loaded (Just texture) ->
            ( { m
                | bushTextures =
                    { bush1 = m.bushTextures.bush1
                    , bush2 = Just texture
                    }
              }
            , Cmd.none
            )

        TextureBush2Loaded Nothing ->
            ( m, Cmd.none )

texturesFromAvSheet : Texture -> PlayerTextures
texturesFromAvSheet avSheet =
    let
        cell : Float
        cell =
            256

        sprite : Int -> Int -> Texture
        sprite x y =
            Texture.sprite
                { x = toFloat x * cell
                , y = toFloat y * cell
                , width = cell
                , height = cell
                }
                avSheet

        downCycle : WalkCycle
        downCycle =
            { frame0 = sprite 0 0
            , frame1 = sprite 1 0
            , frame2 = sprite 2 0
            , frame3 = sprite 3 0
            }

        upCycle : WalkCycle
        upCycle =
            { frame0 = sprite 0 1
            , frame1 = sprite 1 1
            , frame2 = sprite 2 1
            , frame3 = sprite 3 1
            }

        leftCycle : WalkCycle
        leftCycle =
            { frame0 = sprite 0 2
            , frame1 = sprite 1 2
            , frame2 = sprite 2 2
            , frame3 = sprite 3 2
            }

        rightCycle : WalkCycle
        rightCycle =
            { frame0 = sprite 0 3
            , frame1 = sprite 1 3
            , frame2 = sprite 2 3
            , frame3 = sprite 3 3
            }

    in
    { down = downCycle
    , up = upCycle
    , left = leftCycle
    , right = rightCycle
    }




lobNoise : Model -> Cmd Msg
lobNoise m =
    if m.isGatheringLobs
    then Random.generate LobNoiseAvailable (Random.float -0.05 0.05)
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


handleDown : String -> Model -> Model
handleDown code m =
    let
        updatedKeys =
            Set.insert code m.keysDown

        newFacing =
            case code of
                "KeyW" ->
                    FaceUp

                "KeyS" ->
                    FaceDown

                "KeyA" ->
                    FaceLeft

                "KeyD" ->
                    FaceRight

                _ ->
                    m.lastFacing
    in
    { m
        | keysDown = updatedKeys
        , lastFacing = newFacing
    }


handleUp : String -> Model -> Model
handleUp code m =
    { m | keysDown = Set.remove code m.keysDown }


isMoving : Model -> Bool
isMoving m =
    movementVector m /= { x = 0, y = 0 }


movementVector : Model -> WPoint
movementVector m =
    { x =
        (if Set.member "KeyD" m.keysDown then 1 else 0)
        - (if Set.member "KeyA" m.keysDown then 1 else 0)
    , y =
        (if Set.member "KeyW" m.keysDown then 1 else 0)
        - (if Set.member "KeyS" m.keysDown then 1 else 0)
    }


isRunning : Model -> Bool
isRunning m =
    Set.member "ShiftLeft" m.keysDown
        || Set.member "ShiftRight" m.keysDown


view : Model -> Html Msg
view m =
    div []
        [ div [ class "two-canvasses" ]
            [ lifeView m
            , tabletView m
            ]
        , div [] [ text <|
            case m.submittedText of 
                "" -> ""
                _ -> "You submitted: " ++ m.submittedText ]
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
            , tabletButtons
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
        ++ bushesView m.bushTextures m.bushes zoom center
        ++ transmitterView zoom center m.transmitter
        ++ avatarRender zoom center m


tabletScene : Model -> List Renderable
tabletScene m =
    let
        center =
            cameraCenter m
    in
    tabletBckgrd
        ++ bushesView m.bushTextures m.bushes m.zoom center
        ++ transmitterView m.zoom center m.transmitter
        ++ avatarRender m.zoom center m


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



avatarRender : Float -> WPoint -> Model -> List Renderable
avatarRender zoom center m =
    case m.playerTextures of
        Just playerTextures ->
            [ walkingAnimation zoom center m playerTextures ]

        Nothing ->
            []


walkingAnimation : Float -> WPoint -> Model -> PlayerTextures -> Renderable
walkingAnimation zoom center m playerTextures =
    let
        facing =
            currentFacing m

        cycle : WalkCycle
        cycle =
            currentWalkCycle facing playerTextures

        frame : Int
        frame =
            if isMoving m then
                (round m.time // 150) |> remainderBy 4

            else
                idleFrame (currentFacing m)

        texture : Texture
        texture =
            frameTexture frame cycle

    in
    oCZTexture {zoom = zoom, center = center, point = m.player} 256 256 texture


currentFacing : Model -> Facing
currentFacing m =
    let
        moving =
            movementVector m
    in
    if moving.x > 0 then
        FaceRight

    else if moving.x < 0 then
        FaceLeft

    else if moving.y > 0 then
        FaceUp

    else if moving.y < 0 then
        FaceDown

    else
        m.lastFacing


currentWalkCycle : Facing -> PlayerTextures -> WalkCycle
currentWalkCycle facing playerTextures =
    case facing of
        FaceDown ->
            playerTextures.down

        FaceUp ->
            playerTextures.up

        FaceLeft ->
            playerTextures.left

        FaceRight ->
            playerTextures.right


idleFrame : Facing -> Int
idleFrame facing =
    case facing of
        FaceDown ->
            0

        FaceUp ->
            0

        FaceLeft ->
            1

        FaceRight ->
            1


frameTexture : Int -> WalkCycle -> Texture
frameTexture frame cycle =
    case frame of
        0 ->
            cycle.frame0

        1 ->
            cycle.frame1

        2 ->
            cycle.frame2

        _ ->
            cycle.frame3


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
        [ mergeButton 
        , zoomInButton 
        , zoomOutButton
        , messageButton
        ]

        
mergeButton : Html Msg
mergeButton =
    button [ Html.Attributes.title "Merge", class "game-btn" ]
           [ i [ class "fa-solid fa-object-group fa-2x" ] [] ]


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
    button [ Html.Attributes.title "Messages", class "game-btn", onClick ToggleOverlay ]
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
    if m.overlayShown then
        div [ class "overlay" ]
            [ overlayBackButton
            , missionMessage
            ] 

    else
        div [ class "hidden" ] []


overlayBackButton : Html Msg
overlayBackButton =
    button [ Html.Attributes.title "Back", class "backbtn game-btn", onClick ToggleOverlay ]
           [ i [ class "fa-solid fa-chevron-left fa-2x" ] [] ]


missionMessage :  Html Msg
missionMessage =
    div [ class "mission-message" ]
        [ i [ class "fa-regular fa-user fa-3x"] []
        , p [] [text """Use your direction-finding equipment to locate the enemy transmitter.
                
                The adversary's scouts are watching in force. To avoid capture, stay south of the east/west road, which is grid $TODO ADD NORTHING VALUE} northing.
                
                Once you have determined the transmitter's grid location, send it to me using your tablet's submission form. Use either an 8 digit grid coordinate (within 10 meters) or a 10 digit grid coordinate (within 1 meter)""" ] 
        ]


bushGenerator : Random.Generator Bush
bushGenerator =
    Random.map3
        (\x y kind ->
            { position = { x = x, y = y }
            , kind = kind
            }
        )
        (Random.float 69000 71000)
        (Random.float 39000 41000)
        (Random.uniform Bush1 [ Bush2 ])


bushesGenerator : Random.Generator (List Bush)
bushesGenerator =
    Random.list 100 bushGenerator


bushesView : BushTextures -> List Bush -> Float -> WPoint -> List Renderable
bushesView bushTextures bushes zoom center =
    List.map (bushView bushTextures zoom center) bushes


bushView : BushTextures -> Float -> WPoint -> Bush -> Renderable
bushView bushTextures zoom center bush =
    let
        texture =
            case bush.kind of
                Bush1 ->
                    bushTextures.bush1

                Bush2 ->
                    bushTextures.bush2

        size =
            20
    in
    case texture of
        Just tex ->
            oCZTexture
                { zoom = zoom
                , center = center
                , point = bush.position
                }
                size
                size
                tex

        Nothing ->
            shapes [ fill Color.green ]
                [ oCZRect zoom center bush.position (size * zoom) (size * zoom) ]


-- offset centered zoomed image (texture)
avatarSpriteScale : Float -> Float
avatarSpriteScale zoom =
    max 0.15 (0.25 * zoom)


oCZTexture : PosInfo -> Float -> Float -> Texture -> Renderable
oCZTexture posInfo width height texture =
    let
        canvpoint : CPoint
        canvpoint =
            worldToCanvas posInfo

        spriteScale : Float
        spriteScale =
            avatarSpriteScale posInfo.zoom
    in
    Canvas.texture
        [ transform
            [ translate canvpoint.cx canvpoint.cy
            , scale spriteScale spriteScale
            ]
        ]
        ( -width / 2, -height / 2 )
        texture


-- offset centered zoomed rectangle
oCZRect : Float -> WPoint -> WPoint -> Float -> Float -> Shape
oCZRect zoom center wp w h =
    let
        canvpoint : CPoint
        canvpoint = worldToCanvas {zoom = zoom, center = center, point = wp}
        xcentered = canvpoint.cx - w/2
        ycentered = canvpoint.cy - h/2
    in
    rect (xcentered, ycentered) w h


-- offset centered zoomed line
oCZLine : Float -> WPoint -> WPoint -> WPoint -> Shape
oCZLine zoom center begin end =
    let
        canvb = worldToCanvas { zoom = zoom, center = center, point = begin}
        canve = worldToCanvas { zoom = zoom, center = center, point = end}
    in
    drawLineRaw canvb canve


drawLineRaw : CPoint -> CPoint -> Shape
drawLineRaw start end =
    path (start.cx, start.cy) [ lineTo (end.cx, end.cy) ]


updateText : String -> Model -> Model
updateText newText m =
    { m | currentText = newText }


submitText : Model -> Model
submitText m =
    { m
        | submittedText = m.currentText
        , currentText = ""
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
            , value m.currentText
            , onInput TextChanged
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
            (Decode.map KeyDown (Decode.field "code" Decode.string))
        , Browser.Events.onKeyUp
            (Decode.map KeyUp (Decode.field "code" Decode.string))
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
    element
        { init =
            \_ ->
                ( initialModel
                , Random.generate BushesGenerated bushesGenerator
                )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }