-- based on https://github.com/joakin/elm-canvas/blob/master/examples/Textures.elm
module Textures exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Canvas.Settings.Text exposing (..)
import Canvas.Texture as Texture exposing (Texture)
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import Random
import Time exposing (Posix)



main : Program () Model Msg
main =
    Browser.element { init = \_ -> ( initialModel, Cmd.none ), update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta AnimationFrame


h : number
h =
    400


w : number
w =
    600
    

type alias Model =
    { time : Float
    , playerTextures : Maybe PlayerTextures
    , soloImage : Maybe Texture
    }


type alias PlayerTextures =
    { standby : Texture
    , up : Texture
    , down : Texture
    }


type Msg
    = AnimationFrame Float
    | Texture1Loaded (Maybe Texture)
    -- | Texture2Loaded (Maybe Texture)


initialModel : Model
initialModel = { time = 0, playerTextures = Nothing, soloImage = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame delta ->
            ( { model | time = model.time + delta }
            , Cmd.none
            )

        Texture1Loaded Nothing ->
            ( model, Cmd.none )

        Texture1Loaded (Just spriteSheet) ->
            ( { model
                | playerTextures =
                    let
                        cell = 256

                        sprite x y =
                            Texture.sprite
                                { x = x * cell
                                , y = y * cell
                                , width = cell
                                , height = cell
                                }
                                spriteSheet
                    in
                    Just
                        { standby = sprite 1 3 
                        , up = sprite 0 3 
                        , down = sprite 2 3
                        }
              }
            , Cmd.none
            )
        
        -- Texture2Loaded Nothing ->
        --     ( { model | playerTextures = Failure }
        --     , Cmd.none
        --     )
        
        -- Texture2Loaded (Just t) ->
        --     ( { model | soloImage = Just t }
        --     , Cmd.none )


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "../assets/avatar_sheet.png" Texture1Loaded
    -- , Texture.loadFromImageUrl "../assets/tx.png" Texture2Loaded
    ]






view : Model -> Html Msg
view model =
    Canvas.toHtmlWith
        canvasSettings
        []
        ( background
            :: (case model.playerTextures of
                    Just ss ->
                        [ walkingAnimation model.time ss ]

                    Nothing ->
                        []
               )
        )

background : Renderable
background = shapes [ fill (Color.green) ] [ rect ( 0, 0 ) w h ]


canvasSettings = { width = w
        , height = h
        , textures = textures
        }


-- centeredImage : Float -> Float -> Float -> Float -> NotSure
-- centeredImage x y w h =
--     let
--         xcentered = x - w/2
--         ycentered = y - h/2
--     in
--     somethingthatprobablydrawsanimage (xcentered, ycentered) w h


               
zoomedTexture : Float -> Float -> Float -> Texture -> Renderable
zoomedTexture zoom x y t =
    texture [ transform [ scale zoom zoom ] ] ( x, y ) t


walkingAnimation : Float -> PlayerTextures -> Renderable
walkingAnimation time playerTextures =
            let
                thirdsofsec : Int
                thirdsofsec = time |> round |> remainderBy 1000

                t : Texture
                t =
                    if thirdsofsec < 333 then
                        playerTextures.down

                    else if thirdsofsec < 666 then
                        playerTextures.standby

                    else
                        playerTextures.up

            in
            zoomedTexture (time / 5000) 20 200 t
            
            
