module Main exposing (main)

import Browser exposing (Document)
import Debug
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Html exposing (Html)
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { above : List Float
    , below : List Float
    , curr : String
    }


type Msg
    = Push Dir
    | In String
    | Key Keypress


type alias Keypress =
    { key : String
    , shift : Bool
    }


type Dir
    = Up
    | Down


init : () -> ( Model, Cmd Msg )
init _ =
    ( { above = [], below = [], curr = "" }
    , Cmd.none
    )


viewCell : Float -> Element Msg
viewCell expr =
    el
        [ width <| px 300
        , Background.color (rgb 0 0.1 0.5)
        ]
    <|
        text <|
            String.fromFloat expr


view : Model -> Document Msg
view model =
    let
        cellsView : Element Msg
        cellsView =
            Input.text
                [ centerX
                , centerY
                , above <| column [] <| List.map viewCell model.above
                , below <| column [] <| List.map viewCell model.below
                ]
                { label = Input.labelHidden "input"
                , onChange = In
                , placeholder = Nothing
                , text = model.curr
                }

        controlsView : Element Msg
        controlsView =
            wrappedRow
                [ Background.color (rgb 0 0.1 0.5)
                , width <| fillPortion 3
                , height fill
                , padding 10
                , spacing 8
                ]
                [ text "hello world"
                ]

        keyDecoder : Decoder Keypress
        keyDecoder =
            Decode.map2 Keypress
                (Decode.field "key" Decode.string)
                (Decode.field "shiftKey" Decode.bool)

        keyDown : Element.Attribute Msg
        keyDown =
            htmlAttribute
                (Events.on "keydown"
                    (Decode.map Key keyDecoder)
                )

        root : Html Msg
        root =
            layout [ keyDown ] <|
                row
                    [ height fill
                    , width fill
                    ]
                    [ cellsView, controlsView ]
    in
    { title = "hello world", body = [ root ] }


push : Dir -> Model -> Model
push dir model =
    let
        entry =
            String.toFloat model.curr
    in
    case ( entry, dir ) of
        ( Just expr, Up ) ->
            { model | above = expr :: model.above }

        ( Just expr, Down ) ->
            { model | below = expr :: model.below }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Push dir ->
            ( push dir model, Cmd.none )

        In str ->
            ( { model | curr = str }, Cmd.none )

        Key key ->
            let
                updated =
                    case ( Debug.log "key" key.key, key.shift ) of
                        ( "Enter", False ) ->
                            push Up model

                        ( "Enter", True ) ->
                            push Down model

                        _ ->
                            model
            in
            ( updated, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
