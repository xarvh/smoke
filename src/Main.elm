module Main exposing (main)

import AnimationFrame
import Html exposing (..)
import Html.Attributes as HA
import Task
import Time exposing (Time)
import Scene
import WebGL
import Window


type alias Model =
    { time : Time
    , windowSize : Window.Size
    }


type Msg
    = OnAnimationFrame Time
    | OnWindowResizes Window.Size


init =
    ( { time = 0
      , windowSize =
            { width = 100
            , height = 100
            }
      }
    , Task.perform OnWindowResizes Window.size
    )


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnAnimationFrame dt ->
            { model | time = model.time + dt }

        OnWindowResizes size ->
            { model | windowSize = size }


view : Model -> Html Msg
view model =
    let
        -- assuming landscape ratio
        height =
            model.windowSize.height

        width =
            toFloat model.windowSize.height * Scene.viewportWidthHeightRatio |> round
    in
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
                (Scene.entities model.time)
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
