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
    let
        toCmd msg =
            Task.perform identity (Task.succeed msg)

        merge fx ( m1, cmds ) =
            let
                ( m2, newCmds ) =
                    produce fx m1
            in
            ( m2, Cmd.batch [ cmds, newCmds ] )

        update msg model =
            case msg of
                Fact fact ->
                    ( apply fact model, Cmd.none )

                Intent intent ->
                    let
                        ( facts, fxs ) =
                            interpret intent model

                        cmdsFromFacts =
                            Cmd.map Fact (Cmd.batch (List.map toCmd facts))
                    in
                    List.foldl merge ( model, cmdsFromFacts ) fxs
    in
    Browser.application
        { init =
            \f u n ->
                let
                    ( m, fxs ) =
                        init f u n
                in
                List.foldl merge ( m, Cmd.none ) fxs
        , onUrlRequest = Fact << LinkClicked
        , onUrlChange = Fact << UrlChanged
        , subscriptions = subscriptions
        , update = update
        , view =
            \m ->
                let
                    { body, title } =
                        view m
                in
                { title = title
                , body = List.map (Html.map Intent) body
                }
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


init : Flags -> Url -> Nav.Key -> ( Model, List Fx )
init flags url navKey =
    ( { navKey = navKey
      }
    , [ ObtainViewport
      ]
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


type Fx
    = ObtainViewport


interpret : Intent -> Model -> ( List Fact, List Fx )
interpret intent model =
    ( [], [] )


apply : Fact -> Model -> Model
apply fact model =
    model


produce : Fx -> Model -> ( Model, Cmd Msg )
produce fx model =
    case fx of
        ObtainViewport ->
            ( model
            , Task.perform (Fact << ViewportChanged) Browser.Dom.getViewport
            )



-- View


type Intent
    = None


view : Model -> Browser.Document Intent
view model =
    { title = "Game1"
    , body =
        [ Html.text "Hello World!"
        ]
    }
