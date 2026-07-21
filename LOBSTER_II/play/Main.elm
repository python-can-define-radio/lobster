module Main exposing (main)
{-| This module is LOBSTER's (our direction finding game) main file-}

import Browser exposing (element)
import Browser.Events
import Canvas exposing (Renderable, Shape, shapes, clear)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Line exposing (lineWidth)
import Color
import Canvas.Texture exposing (Texture)
import Canvas
import Canvas.Texture as Texture
import Html exposing (Html, button, div, form, i, input, p, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode
import Random
import Set exposing (Set)
import Char

import Supp exposing (posText, keepNonNothing, currentFacing, isMoving, azimuthFromPositions, azToDeg, Azimuth, drawLine, cRect, cTexture, transmitterCoordinatesText, movementVector, WPoint, lobIntervalMs, decodeMouseMovement, format2dp, snapToGrid, gridSpacing, texturesFromAvSheet, frameTexture, idleFrame, currentWalkCycle, PlayerTextures, WalkCycle, Facing(..), canvW, canvH, lifeBckgrd, tabletBckgrd)


type alias Model =
    { playerPos : WPoint
    , dirx : Float
    , diry : Float
    , lobs : List Lob
    , transmitter : WPoint
    , panCenter : Maybe WPoint
    , isMouseDown : Bool
    , messagesShown : Bool
    , zoom : Float
    , formInputValue : String
    , formSubmittedValue : String
    , isGatheringLobs : Bool
    , time : Float
    , lastFacing : Facing
    , keysDown : Set String
    , localTextures : LocalTextures
    , objects : List SimpleOb
    , playerTextures : Maybe PlayerTextures
    , timeSinceLastLob : Float
    , mergedView : Bool
    , submissionPopupShown : Bool
    , selectedLob : Maybe Lob
    }

type alias LocalTextures =
    { player : Maybe PlayerTextures
    , bush1 : Maybe Texture
    , bush2 : Maybe Texture
    , road : Maybe Texture 
    , roadnolines : Maybe Texture
    }


type TexName
    = Bush1
    | Bush2
    | Road
    | RoadNoLines

{-| `name` refers to the texture. There may be more than 
one SimpleOb with the same `name`. -}
type alias SimpleOb =
    { name : TexName
    , position : WPoint
    -- , TODO rotDeg : Int
    }


type GridPrecision
    = Grid8
    | Grid10


type alias CDiff =
    { cdx : Float
    , cdy : Float
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
    | ToggleOverlay
    | ZoomIn
    | ZoomOut
    | Recenter
    | ClearLobs
    | TextChanged String
    | Submit
    | CloseSubmissionPopup
    | ToggleGatherLobs
    | ToggleMerge
    | SelectLob (Float, Float)
    | CloseLobDialog
    | LobNoiseAvailable Float
    | BushesGenerated (List SimpleOb)
    | TextureAvSheetLoaded (Maybe Texture)
    | TextureLoaded TexName (Maybe Texture)


initialModel : Model
initialModel =
    { playerPos = { x = 70100, y = 40100 }
    , dirx = 0
    , diry = 0
    , lobs = []
    , transmitter = { x = 70228, y = 41005 }
    , panCenter = Nothing
    , isMouseDown = False
    , messagesShown = False
    , zoom = 1.0
    , formInputValue = ""
    , formSubmittedValue = ""
    , isGatheringLobs = True
    , time = 0
    , lastFacing = FaceDown
    , keysDown = Set.empty
    , playerTextures = Nothing
    , localTextures = 
        { player = Nothing
        , bush1 = Nothing
        , bush2 = Nothing
        , road = Nothing 
        , roadnolines = Nothing
        }
    , objects = roads
    , timeSinceLastLob = 0
    , mergedView = False
    , submissionPopupShown = False
    , selectedLob = Nothing
    }


distperMs : Float
distperMs = 0.05


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "../assets/avatar_sheet2.png" TextureAvSheetLoaded
    , Texture.loadFromImageUrl "../assets/bush_1.png" (TextureLoaded Bush1)
    , Texture.loadFromImageUrl "../assets/bush_2.png" (TextureLoaded Bush2)
    , Texture.loadFromImageUrl "../assets/road.png" (TextureLoaded Road)
    , Texture.loadFromImageUrl "../assets/road_no_lines.png" (TextureLoaded RoadNoLines)
    ]
    
move : Float -> Model -> Model
move dt m =
    let
        movement =
            movementVector m.keysDown

        speed =
            if isRunning m then
                distperMs * 2
            else
                distperMs
    in
    { m
        | playerPos =
            { x = m.playerPos.x + movement.xdir * speed * dt
            , y = m.playerPos.y + movement.ydir * speed * dt
            }
    }

makeLob : Float -> WPoint -> WPoint -> Lob
makeLob noise player transmitter =
    let
        base =
            azimuthFromPositions player transmitter

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
            player.x - transmitter.x

        dy =
            player.y - transmitter.y

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
    { source = player
    , azimuth = rotated
    , power = { mW = power }
    }


accumulateTime : Float -> Model -> Model
accumulateTime dt m =
    { m
        | time = m.time + dt
        , timeSinceLastLob = m.timeSinceLastLob + dt
    }


cameraCenter : Model -> WPoint
cameraCenter m =
    case m.panCenter of
        Just center ->
            center

        Nothing ->
            m.playerPos


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
            ( { m
                | isMouseDown = True
                , selectedLob = secondLob m.lobs
              }
            , Cmd.none
            )

        MouseUp ->
            ( { m | isMouseDown = False }, Cmd.none )

        MouseMove (dx, dy) ->
            ( handleMouseMove {cdx = dx, cdy = dy} m, Cmd.none )

        SelectLob mousePoint ->
            ( { m | selectedLob = findSelectedLob m mousePoint }
            , Cmd.none
            )

        CloseLobDialog ->
            ( { m | selectedLob = Nothing }
            , Cmd.none
            )

        ToggleOverlay ->
            ( { m | messagesShown = not m.messagesShown }, Cmd.none )

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

        ToggleMerge ->
            ( { m | mergedView = not m.mergedView }, Cmd.none )

        LobNoiseAvailable noise ->
            ( { m
                | lobs = 
                    makeLob noise m.playerPos m.transmitter
                    :: m.lobs
                , timeSinceLastLob = 0
              }
            , Cmd.none
            )

        TextChanged newText ->
            ( { m | formInputValue = newText }, Cmd.none )

        Submit ->
            ( { m
                | formSubmittedValue = m.formInputValue
                , formInputValue = ""
                , submissionPopupShown = True
                }
            , Cmd.none
            )

        CloseSubmissionPopup ->
            ( { m | submissionPopupShown = False }, Cmd.none )

        TextureAvSheetLoaded (Just avSheet) ->
            ( { m | playerTextures = Just (texturesFromAvSheet avSheet)}
            , Cmd.none
            )
        
        TextureAvSheetLoaded Nothing ->
            ( m, Cmd.none )

        BushesGenerated bushes ->
            ( { m | objects = bushes ++ m.objects }, Cmd.none )
        
        TextureLoaded texName tex ->
            ( { m | localTextures =
                handleTextureLoaded m.localTextures texName tex
              }, Cmd.none ) 


handleTextureLoaded : LocalTextures -> TexName -> Maybe Texture -> LocalTextures
handleTextureLoaded localTextures texName tex =
    case texName of
        Bush1 -> 
            { localTextures | bush1 = tex }
        
        Bush2 -> 
            { localTextures | bush2 = tex }

        Road -> 
            { localTextures | road = tex }

        RoadNoLines -> 
            { localTextures | roadnolines = tex }
            


lobNoise : Model -> Cmd Msg
lobNoise m =
    if m.isGatheringLobs && m.timeSinceLastLob >= lobIntervalMs then
        Random.generate LobNoiseAvailable (Random.float -0.05 0.05)

    else
        Cmd.none


secondLob : List Lob -> Maybe Lob
secondLob lobs =
    case lobs of
        _ :: second :: _ ->
            Just second

        _ ->
            Nothing


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


isRunning : Model -> Bool
isRunning m =
    Set.member "ShiftLeft" m.keysDown


view : Model -> Html Msg
view m =
    div []
        [ div [ class "two-canvasses" ]
            (canvasViews m)
        ]


canvasViews : Model -> List (Html Msg)
canvasViews m =
    if m.mergedView then
        [ tabletView m ]

    else
        [ lifeView m
        , tabletView m
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


lobCanvasAttributes : List (Html.Attribute Msg)
lobCanvasAttributes =
    [ class "lobs-canvas"
    , Html.Events.onMouseDown MouseDown
    ]


tabletView : Model -> Html Msg
tabletView m =
    div [ class "hudwrap" ]
        [ div [ class "hud" ]
            [ Canvas.toHtml (canvW, canvH)
                [ class "tablet-canvas" ]
                (tabletScene m)
            , Canvas.toHtml (canvW, canvH)
                lobCanvasAttributes
                (lobsScene m)
            , posText m.playerPos
            , tabletButtons
            , clearLobsButton
            , gatherLobsButton m
            , recenterButton m
            , messagesOverlay m
            , lobPowerView m
            , inputForm m
            , submissionPopup m
            , lobDialog m
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
            m.playerPos

        zoom =
            1
    in
    lifeBckgrd
        ++ objectsView m.localTextures m.objects zoom center
        ++ transmitterView zoom center m.transmitter
        ++ avatarRender zoom center m


tabletScene : Model -> List Renderable
tabletScene m =
    let
        center =
            cameraCenter m
    in
    tabletBckgrd
        ++ gridView m.zoom center
        ++ objectsView m.localTextures m.objects m.zoom center
        ++ transmitterView m.zoom center m.transmitter
        ++ avatarRender m.zoom center m


lobsScene : Model -> List Renderable
lobsScene m =
    let
        center =
            cameraCenter m
    in
        ( clear ( 0, 0 ) (toFloat canvW) (toFloat canvH) )
            :: lobsView m.zoom center m.lobs m.selectedLob


lobsView : Float -> WPoint -> List Lob -> Maybe Lob -> List Renderable
lobsView zoom center lobs selected =
    let
        unselected =
            List.filter ((/=) selected) (List.map Just lobs)
                |> List.filterMap identity

        selectedRenderable =
            case selected of
                Just lob ->
                    [ selectedLobRenderable zoom center lob ]

                Nothing ->
                    []
    in
    case unselected of
        [] ->
            selectedRenderable

        newest :: older ->
            olderLobsRenderable zoom center older
                ++ (newestLobRenderable zoom center newest
                :: selectedRenderable)


newestLobRenderable : Float -> WPoint -> Lob -> Renderable
newestLobRenderable zoom center lob =
    shapes
        [ stroke Color.red
        , lineWidth 3
        ]
        [ drawLine
            zoom
            center
            lob.source
            (lobEndpoint lob)
        ]


olderLobsRenderable : Float -> WPoint -> List Lob -> List Renderable
olderLobsRenderable zoom center lobs =
    List.map
        (olderLobRenderable zoom center)
        lobs


olderLobRenderable : Float -> WPoint -> Lob -> Renderable
olderLobRenderable zoom center lob =
    shapes
        [ stroke Color.orange
        , lineWidth 2
        ]
        [ olderLobShape zoom center lob ]


olderLobShape : Float -> WPoint -> Lob -> Shape
olderLobShape zoom center lob =
    drawLine
        zoom
        center
        lob.source
        (lobEndpoint lob)


selectedLobRenderable : Float -> WPoint -> Lob -> Renderable
selectedLobRenderable zoom center lob =
    shapes
        [ stroke Color.blue
        , lineWidth 4
        ]
        [ drawLine
            zoom
            center
            lob.source
            (lobEndpoint lob)
        ]


lobLength : Float
lobLength =
    50000


lobEndpoint : Lob -> WPoint
lobEndpoint lob =
    { x = lob.source.x + lob.azimuth.cosresult * lobLength
    , y = lob.source.y + lob.azimuth.sinresult * lobLength
    }


transmitterView : Float -> WPoint -> WPoint -> List Renderable
transmitterView zoom center p =
    let 
        posInfo =
            { zoom = zoom
            , center = center
            , point = p
            }
    in 
    [ shapes [ fill Color.red ]
        [ cRect posInfo 20 20 ]
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
            currentFacing m.keysDown m.lastFacing

        cycle : WalkCycle
        cycle =
            currentWalkCycle facing playerTextures

        frame : Int
        frame =
            if isMoving m.keysDown then
                (round m.time // 150) |> remainderBy 4

            else
                idleFrame <| currentFacing m.keysDown m.lastFacing

        texture : Texture
        texture =
            frameTexture frame cycle

    in
    cTexture {zoom = zoom, center = center, point = m.playerPos} 0.2 0.1 texture



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
    button
        [ Html.Attributes.title "Merge"
        , class "game-btn"
        , onClick ToggleMerge
        ]
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
    if m.messagesShown then
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


pointGen : Random.Generator WPoint
pointGen =
    Random.map2
        WPoint 
        (Random.float 69000 71000)
        (Random.float 39000 41000)


roads : List SimpleOb
roads =
    let
        makeroad : Float -> Float -> SimpleOb
        makeroad x y = { name = Road, position = { x = x, y = y } }

        verticalroad a b =
            List.range a b             -- if a=0 and b=3:  [0, 1, 2, 3]
            |> List.map ((*)150)    -- [0, 150, 300, 450]
            |> List.map ((+)40200)  -- [40200, 40350, etc]
            |> List.map toFloat
            |> List.map (\y -> makeroad 70100 y)

    in
        (verticalroad -6 -2) ++ [{ name = RoadNoLines, position = { x = 70100, y = 40050 } }] ++ (verticalroad 0 4)

        
bushGenerator : Random.Generator SimpleOb
bushGenerator =
    Random.map2
        SimpleOb
        (Random.uniform Bush1 [ Bush2 ])
        pointGen


bushesGenerator : Random.Generator (List SimpleOb)
bushesGenerator =
    Random.list 100 bushGenerator


gridView : Float -> WPoint -> List Renderable
gridView zoom center =
    let
        spacing =
            gridSpacing zoom

        far =
            spacing * 40

        xStart =
            snapToGrid spacing (center.x - far)

        yStart =
            snapToGrid spacing (center.y - far)

        xEnd =
            xStart + far * 2

        yEnd =
            yStart + far * 2
    in
    [ shapes
        [ stroke (Color.rgba 0.8 0.8 0.8 0.5) ]
        (gridVerticalLines zoom center spacing xStart xEnd yStart yEnd
            ++ gridHorizontalLines zoom center spacing xStart xEnd yStart yEnd)
    ]


gridVerticalLines :
    Float
    -> WPoint
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> List Shape
gridVerticalLines zoom center spacing x xEnd yStart yEnd =
    if x > xEnd then
        []

    else
        drawLine zoom center
            { x = x, y = yStart }
            { x = x, y = yEnd }
            :: gridVerticalLines
                zoom
                center
                spacing
                (x + spacing)
                xEnd
                yStart
                yEnd


gridHorizontalLines :
    Float
    -> WPoint
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> List Shape
gridHorizontalLines zoom center spacing xStart xEnd y yEnd =
    if y > yEnd then
        []

    else
        drawLine zoom center
            { x = xStart, y = y }
            { x = xEnd, y = y }
            :: gridHorizontalLines
                zoom
                center
                spacing
                xStart
                xEnd
                (y + spacing)
                yEnd


objectsView : LocalTextures -> List SimpleOb -> Float -> WPoint -> List Renderable
objectsView localTextures objects zoom center =
    objects
        |> List.map (objectView localTextures zoom center)
        |> keepNonNothing


nameToTexture : LocalTextures -> TexName -> Maybe Texture
nameToTexture localTextures texName =
    case texName of
        Bush1 ->
            localTextures.bush1

        Bush2 ->
            localTextures.bush2

        Road ->
            localTextures.road

        RoadNoLines ->
            localTextures.roadnolines


objectView : LocalTextures -> Float -> WPoint -> SimpleOb -> Maybe Renderable
objectView localTextures zoom center object =
    let
        tex = nameToTexture localTextures object.name
        
        posInfo = 
            { zoom = zoom
            , center = center
            , point = object.position
            }
        
        mb f = Maybe.map f tex

    in
    mb <| cTexture posInfo 0.15 0.001


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
            , value m.formInputValue
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


viewLobPower : Lob -> Html Msg
viewLobPower lob =
    div []
        [ text ("Power: " ++ format2dp lob.power.mW ++ " dB") ]


submissionPopup : Model -> Html Msg
submissionPopup m =
    if m.submissionPopupShown then
        submissionDialog m

    else
        div [ class "hidden" ] []


submissionDialog : Model -> Html Msg
submissionDialog m =
    div
        [ class "game-dialog" ]
        [ p [] [ text ("DEBUG: The Tx coordinates are: " ++ transmitterCoordinatesText m.transmitter) ]
        , submissionResultText m
        , submissionDialogButtons
        ]


lobDialog : Model -> Html Msg
lobDialog m =
    case m.selectedLob of
        Just lob ->
            div
                [ class "lob-dialog" ]
                [ p []
                    [ text ("Source: "
                        ++ String.fromInt (round lob.source.x)
                        ++ ", "
                        ++ String.fromInt (round lob.source.y)
                      )
                    ]
                , p []
                    [ text ("Azimuth: "
                        ++ azToDeg lob.azimuth
                        ++ "°")
                    ]
                , p []
                    [ text ("Power: "
                        ++ format2dp lob.power.mW
                        ++ " dB")
                    ]
                , lobDialogButtons
                ]

        Nothing ->
            div [ class "hidden" ] []


lobDialogButtons : Html Msg
lobDialogButtons =
    div
        [ Html.Attributes.id "lob-dialog-button"
        , class "lob-dialog-button"
        ]
        [ button
            [ class "game-btn"
            , onClick CloseLobDialog
            ]
            [ text "OK" ]
        ]


decodeCanvasClick : Decode.Decoder (Float, Float)
decodeCanvasClick =
    Decode.map2 Tuple.pair
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)


findSelectedLob : Model -> (Float, Float) -> Maybe Lob
findSelectedLob m mouse =
    let
        center =
            cameraCenter m

        closest = m.lobs -- TEMPORARY: DONT HANDLE CLOSENESS
            -- List.filter
            --    (lobIsNearClick m.zoom center mouse)
            --    m.lobs
    in
    List.head closest


lobIsNearClick : Float -> WPoint -> (Float, Float) -> Lob -> Bool
lobIsNearClick zoom center mouse lob =
    let
        endpoint =
            lobEndpoint lob

        startScreen =
            screenPoint zoom center lob.source

        endScreen =
            screenPoint zoom center endpoint

        distance =
            distanceToSegment mouse startScreen endScreen
    in
    distance < 8


screenPoint : Float -> WPoint -> WPoint -> WPoint
screenPoint zoom center point =
    { x = (point.x - center.x) * zoom + canvW / 2
    , y = (point.y - center.y) * zoom + canvH / 2
    }


distanceToSegment : (Float, Float) -> WPoint -> WPoint -> Float
distanceToSegment mouse start finish =
    let
        dx =
            finish.x - start.x

        dy =
            finish.y - start.y

        lengthSquared =
            dx * dx + dy * dy

        t =
            if lengthSquared == 0 then
                0

            else
                ((Tuple.first mouse - start.x) * dx
                    + (Tuple.second mouse - start.y) * dy)
                    / lengthSquared

        clamped =
            max 0 (min 1 t)

        closestX =
            start.x + clamped * dx

        closestY =
            start.y + clamped * dy
    in
    sqrt
        ((Tuple.first mouse - closestX) ^ 2
            + (Tuple.second mouse - closestY) ^ 2)


submissionDialogButtons : Html Msg
submissionDialogButtons =
    div
        [ Html.Attributes.id "game-dialog-button" ]
        [ button
            [ class "game-btn"
            , onClick CloseSubmissionPopup
            ]
            [ text "OK" ]
        ]


submissionResultText : Model -> Html Msg
submissionResultText m =
    case submittedCoordinate m of
        Just point ->
            if transmitterMatches point m.transmitter then
                p [ class "col-green" ]
                    [ text "Congratulations! You found the transmitter." ]

            else
                p [ class "col-red" ]
                    [ text "That position does not match the transmitter's location." ]

        Nothing ->
                p [ class "col-red" ]
                    [ text "Please enter a valid 8- or 10-digit grid coordinate." ]
            


submittedCoordinate : Model -> Maybe ( WPoint, GridPrecision )
submittedCoordinate m =
    parseGridCoordinate m.formSubmittedValue


parseGridCoordinate : String -> Maybe ( WPoint, GridPrecision )
parseGridCoordinate text =
    let
        digits =
            String.filter Char.isDigit text
    in
    case String.length digits of
        8 ->
            case parseCoordinateParts8
                (String.left 4 digits)
                (String.dropLeft 4 digits) of
                Just point ->
                    Just ( point, Grid8 )

                Nothing ->
                    Nothing

        10 ->
            case parseCoordinateParts10
                (String.left 5 digits)
                (String.dropLeft 5 digits) of
                Just point ->
                    Just ( point, Grid10 )

                Nothing ->
                    Nothing

        _ ->
            Nothing


parseCoordinateParts8 : String -> String -> Maybe WPoint
parseCoordinateParts8 east north =
    case ( String.toFloat east, String.toFloat north ) of
        ( Just e, Just n ) ->
            Just
                { x = e * 10
                , y = n * 10
                }

        _ ->
            Nothing


parseCoordinateParts10 : String -> String -> Maybe WPoint
parseCoordinateParts10 east north =
    case ( String.toFloat east, String.toFloat north ) of
        ( Just e, Just n ) ->
            Just
                { x = e
                , y = n
                }

        _ ->
            Nothing


transmitterMatches : ( WPoint, GridPrecision ) -> WPoint -> Bool
transmitterMatches ( submitted, precision ) transmitter =
    case precision of
        Grid10 ->
            submitted.x == transmitter.x
                && submitted.y == transmitter.y

        Grid8 ->
            abs (submitted.x - transmitter.x) <= 10
                && abs (submitted.y - transmitter.y) <= 10



subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Decode.map KeyDown (Decode.field "code" Decode.string))
        , Browser.Events.onKeyUp
            (Decode.map KeyUp (Decode.field "code" Decode.string))
        , Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onMouseMove
            (Decode.map MouseMove decodeMouseMovement)
        , Browser.Events.onMouseUp
            (Decode.succeed MouseUp)
        ]


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