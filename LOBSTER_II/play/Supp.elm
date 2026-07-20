-- things that we have 'solved' (i.e. we don't have to think about them,
-- and the AI doesn't need access to them).
module Supp exposing (..)
{-| This module is our direction finding game's Supp file.-}

import Canvas exposing (Renderable, Shape, lineTo, path, rect, shapes)
import Color
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (transform, scale, translate)
import Canvas.Texture exposing (Texture)
import Canvas.Texture as Texture
import Json.Decode as Decode
import Set exposing (Set)


canvW : number
canvW = 600

canvH : number
canvH = 400


type Facing
    = FaceDown
    | FaceUp
    | FaceLeft
    | FaceRight



type alias WPoint =
    { x : Float
    , y : Float
    }

type alias MoveVec = 
    { xdir: Float
    , ydir: Float
    }

type alias Azimuth =
    { sinresult : Float
    , cosresult : Float
    }


type alias PlayerTextures =
    { down : WalkCycle
    , up : WalkCycle
    , left : WalkCycle
    , right : WalkCycle
    }


type alias WalkCycle =
    { frame0 : Texture
    , frame1 : Texture
    , frame2 : Texture
    , frame3 : Texture
    }


{-| Drawing functions which take a `PosInfo` parameter should...
    * use `posInfo.point` as the center of what is rendered (not the topleft corner)  
    * offset and zoom based on `center` and `zoom` -}
type alias PosInfo =
    { zoom : Float
    , center : WPoint
    , point : WPoint
    }

    
type alias CPoint =
    { cx : Float
    , cy : Float
    }


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


gridSpacing : Float -> Float
gridSpacing zoom =
    if zoom < 0.02 then
        16000

    else if zoom < 0.2 then
        1600

    else if zoom < 2 then
        160

    else
        16


snapToGrid : Float -> Float -> Float
snapToGrid spacing value =
    toFloat (floor (value / spacing)) * spacing


avatarSpriteScale : Float -> Float
avatarSpriteScale zoom =
    max 0.15 (0.25 * zoom)


format2dp : Float -> String
format2dp value =
    String.fromFloat (toFloat (round (value * 100)) / 100)


drawLineRaw : CPoint -> CPoint -> Shape
drawLineRaw start end =
    path (start.cx, start.cy) [ lineTo (end.cx, end.cy) ]


decodeMouseMovement : Decode.Decoder (Float, Float)
decodeMouseMovement =
    Decode.map2 Tuple.pair
        (Decode.field "movementX" Decode.float)
        (Decode.field "movementY" Decode.float)


lobIntervalMs : Float
lobIntervalMs =
    400


movementVector : Set String -> MoveVec
movementVector keysDown =
    let
        oneIfDown x = if (Set.member x keysDown) then 1 else 0
    in
        { xdir = oneIfDown "KeyD" - oneIfDown "KeyA"
        , ydir = oneIfDown "KeyW" - oneIfDown "KeyS"
        }


isMoving : Set String -> Bool
isMoving keysDown =
    (movementVector keysDown) /= { xdir = 0, ydir = 0 }


transmitterCoordinatesText : WPoint -> String
transmitterCoordinatesText transmitter =
    String.join
        " "
        (transmitterCoordinateParts transmitter)


transmitterCoordinateParts : WPoint -> List String
transmitterCoordinateParts transmitter =
    [ String.fromInt (round transmitter.x)
    , String.fromInt (round transmitter.y)
    ]


worldToCanvas : PosInfo -> CPoint
worldToCanvas {zoom, center, point} =
    { cx = zoom * (point.x - center.x) + canvW / 2
    , cy = zoom * (center.y - point.y) + canvH / 2
    }


{-| custom image (texture). See docstring on PosInfo. -}
cTexture : PosInfo -> Float -> Float -> Texture -> Renderable
cTexture posInfo imgScale minSize tex =
    let
        canvpoint : CPoint
        canvpoint =
            worldToCanvas posInfo

        z : Float
        z = clamp minSize 99999 <| posInfo.zoom * imgScale

        dims = Texture.dimensions tex
        halfw = dims.width * z / 2
        halfh = dims.height * z / 2
        xcent = canvpoint.cx - halfw
        ycent = canvpoint.cy - halfh

    in
    Canvas.texture
        [ transform
            [ translate xcent ycent
            , scale z z
            ]
        ]
        (0, 0) -- the x, y is in the `translate`
        tex


{-| custom rectangle. See docstring on PosInfo. -}
cRect : PosInfo -> Float -> Float -> Shape
cRect posInfo w h =
    let
        canvpoint : CPoint
        canvpoint = worldToCanvas posInfo
        xcentered = canvpoint.cx - w/2
        ycentered = canvpoint.cy - h/2
    in
    rect (xcentered, ycentered) (w * posInfo.zoom) (h * posInfo.zoom)


{-| Uses the same transformations as cTexture and cRect -}
drawLine : Float -> WPoint -> WPoint -> WPoint -> Shape
drawLine zoom center begin end =
    let
        canvb = worldToCanvas { zoom = zoom, center = center, point = begin}
        canve = worldToCanvas { zoom = zoom, center = center, point = end}
    in
    drawLineRaw canvb canve


degToRad : Float -> Float
degToRad x = x * 180 / pi 

azToDeg : Azimuth -> String
azToDeg azimuth =
    let
        angle = degToRad <| atan2 azimuth.sinresult azimuth.cosresult
        northZero = 90 - angle
        positiveOnly = 
            if northZero < 0
            then northZero + 360
            else northZero
    in
    format2dp positiveOnly


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

currentFacing : Set String -> Facing -> Facing
currentFacing keysDown lastFacing =
    let
        moving =
            movementVector keysDown
    in
    if moving.xdir > 0 then
        FaceRight

    else if moving.xdir < 0 then
        FaceLeft

    else if moving.ydir > 0 then
        FaceUp

    else if moving.ydir < 0 then
        FaceDown

    else
        lastFacing


keepNonNothing : List (Maybe a) -> List a
keepNonNothing lst =
    let 
        keepIfGood : Maybe a -> List (Maybe a) -> List a
        keepIfGood x xs =
            case x of
                Nothing ->
                    keepNonNothing xs
                
                Just item ->
                    item :: keepNonNothing xs
    in
        case lst of
            [] ->
                []

            (x :: xs) ->
                keepIfGood x xs
