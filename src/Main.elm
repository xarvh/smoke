module Main exposing (main)

import AnimationFrame
import Html exposing (..)
import Html.Attributes as HA
import Task
import Time exposing (Time)
import Scene
import WebGL
import WebGL.Texture exposing (Texture)
import Window


type alias Model =
    { time : Time
    , windowSize : Window.Size
    , textureResult : Result String Texture
    }


type Msg
    = OnAnimationFrame Time
    | OnWindowResizes Window.Size
    | OnLoadTexturesResult (Result String Texture)


init =
    ( { time = 0
      , windowSize =
            { width = 100
            , height = 100
            }
      , textureResult = Err "Loading..."
      }
    , Cmd.batch
        [ Task.perform OnWindowResizes Window.size
        , loadTexture
        ]
    )


loadTexture : Cmd Msg
loadTexture =
    let
        defaultOptions =
            WebGL.Texture.defaultOptions

        options =
            { defaultOptions
                | magnify = WebGL.Texture.nearest
                , minify = WebGL.Texture.nearest
            }
    in
        "../assets/texture.jpg"
            |> WebGL.Texture.loadWith options
            |> Task.mapError toString
            |> Task.attempt OnLoadTexturesResult


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnAnimationFrame dt ->
            { model | time = model.time + dt }

        OnWindowResizes size ->
            { model | windowSize = size }

        OnLoadTexturesResult result ->
            { model | textureResult = result }


view : Model -> Html Msg
view model =
    let
        -- assuming landscape ratio
        height =
            model.windowSize.height

        width =
            toFloat model.windowSize.height * Scene.viewportWidthHeightRatio |> round
    in
        case model.textureResult of
            Err message ->
                text message

            Ok texture ->
                div
                    [ HA.style
                        [ ( "display", "flex" )
                        , ( "justify-content", "center" )
                        ]
                    ]
                    [ WebGL.toHtml
                        [ HA.width width
                        , HA.height height
                        ]
                        (Scene.entities texture model.time)
                    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs OnAnimationFrame
        , Window.resizes OnWindowResizes
        ]


main =
    Html.program
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = subscriptions
        }
