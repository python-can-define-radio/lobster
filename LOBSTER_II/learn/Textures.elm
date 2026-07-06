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


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrameDelta Tick

canvH : number
canvH = 400

canvW : number
canvW = 600
    
type alias Model =
    { time : Float
    , playerTextures : Maybe PlayerTextures
    }

type alias PlayerTextures =
    { standby : Texture
    , lff : Texture
    , rff : Texture
    }

type Msg
    = Tick Float
    | TextureAvSheetLoaded (Maybe Texture)


initialModel : Model
initialModel = { time = 0, playerTextures = Nothing }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model | time = model.time + delta }
            , Cmd.none
            )

        TextureAvSheetLoaded Nothing ->
            ( model, Cmd.none )

        TextureAvSheetLoaded (Just avSheet) ->
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
                                avSheet
                    in
                    Just
                        { standby = sprite 1 3 
                        , lff = sprite 0 3 
                        , rff = sprite 2 3
                        }
              }
            , Cmd.none
            )


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "../assets/avatar_sheet.png" TextureAvSheetLoaded
    ]


view : Model -> Html Msg
view m =
    Canvas.toHtmlWith canvasSettings [] ( lifeScene m )

lifeScene : Model -> List Renderable
lifeScene m = background ++ player m


player : Model -> List Renderable
player m =
    case m.playerTextures of
        Just avSheet ->
            [ walkingAnimation m.time avSheet ]

        Nothing ->
            []
        

background : List Renderable
background = [ shapes [ fill (Color.green) ] [ rect ( 0, 0 ) canvW canvH ] ]


canvasSettings = { width = canvW
        , height = canvH
        , textures = textures
        }

     
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
                        playerTextures.rff

                    else if thirdsofsec < 666 then
                        playerTextures.standby

                    else
                        playerTextures.lff

            in
            zoomedTexture (time / 5000) 20 (200 - time / 500) t
            
            
main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view 
        , subscriptions = subscriptions
        }
