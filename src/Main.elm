module Main exposing (main)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html, div, text)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { cells : Dict Int Expr
    , pos : Int
    }


type Expr
    = Num Float


type Msg
    = Push String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cells = Dict.empty, pos = 0 }
    , Cmd.none
    )


view : Model -> Document Msg
view model =
    let
        elem =
            [ Html.text "hello world" ]
    in
    { title = "hello world", body = elem }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Push str ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
