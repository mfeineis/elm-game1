module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (Visibility)
import Browser.Navigation as Nav
import Html
import Task
import Url exposing (Url)


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = Fact << LinkClicked
        , onUrlChange = Fact << UrlChanged
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> Fact (WindowSizeChanged { width = w, height = h }))
        , Browser.Events.onVisibilityChange (Fact << VisibilityChanged)
        ]


type alias Model =
    { navKey : Nav.Key
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    ( { navKey = navKey
      }
    , Task.perform (Fact << ViewportChanged) Browser.Dom.getViewport
    )



-- Domain


type Msg
    = Fact Fact
    | Intent Intent


type Fact
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | ViewportChanged Viewport
    | VisibilityChanged Visibility
    | WindowSizeChanged { width : Int, height : Int }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fact fact ->
            let
                ( m, cmds ) =
                    replay fact model
            in
            ( m, Cmd.map Fact cmds )

        Intent intent ->
            let
                ( m, cmds ) =
                    interpret intent model
            in
            ( m, Cmd.map Fact (Cmd.batch (List.map toCmd cmds)) )


replay : Fact -> Model -> ( Model, Cmd Fact )
replay fact model =
    ( model, Cmd.none )


interpret : Intent -> Model -> ( Model, List Fact )
interpret intent model =
    ( model, [] )



-- View


type Intent
    = None


view : Model -> Browser.Document Msg
view model =
    { title = "Game1"
    , body =
        [ Html.text "Hello World!"
        ]
    }



-- Utils


toCmd : msg -> Cmd msg
toCmd msg =
    Task.perform identity (Task.succeed msg)
