module Main exposing (main)

import AssocList as Dict exposing (Dict)
import Browser exposing (Document)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Element.Keyed as Keyed
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Math exposing (Expression(..), Function)


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
    = Do Action
    | In String
    | Key Keypress


type alias Keypress =
    { key : String, shift : Bool }


type Action
    = Push Dir
    | Pop Dir
    | Application Function


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
        [ width fill
        , height <| px 48
        , padding 12
        , Background.color (rgb 0.9 0.9 0.9)
        ]
        (text <| String.fromFloat expr)


view : Model -> Document Msg
view model =
    let
        viewCells : String -> List Float -> List ( String, Element Msg )
        viewCells prefix =
            List.indexedMap (\i cell -> ( prefix ++ String.fromInt i, viewCell cell ))

        inputDecoder : Decoder ( Msg, Bool )
        inputDecoder =
            Decode.map (\keypress -> ( Key keypress, Dict.member keypress keymap )) <|
                Decode.map2
                    Keypress
                    (Decode.field "key" Decode.string)
                    (Decode.field "shiftKey" Decode.bool)

        input : Element Msg
        input =
            Input.text
                [ htmlAttribute <| Html.Events.preventDefaultOn "keydown" inputDecoder ]
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

        root : Html Msg
        root =
            layout [] <|
                row
                    [ height fill
                    , width fill
                    ]
                    [ Keyed.column
                        [ width <| fillPortion 2
                        , alignTop
                        ]
                      <|
                        List.concat
                            [ List.reverse (viewCells "above" model.above)
                            , [ ( "input", input ) ]
                            , viewCells "below" model.below
                            ]
                    , controlsView
                    ]
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


keymap : Dict Keypress Action
keymap =
    Dict.fromList
        [ ( Keypress "Enter" False, Push Up )
        , ( Keypress "Enter" True, Push Down )
        , ( Keypress " " False, Pop Up )
        , ( Keypress " " True, Pop Down )
        , ( Keypress "+" True, Application Math.add )
        , ( Keypress "*" True, Application Math.mul )
        , ( Keypress "/" False, Application Math.div )
        ]


applied : Function -> Model -> Model
applied function model =
    let
        args =
            List.take function.args model.above

        evaluated =
            Math.eval <| Apply function (List.map Number args)
    in
    case evaluated of
        Ok (Number n) ->
            { model
                | curr = Debug.log "eval" <| String.fromFloat n
                , above = n :: List.drop function.args model.above
            }

        _ ->
            model


perform : Action -> Model -> Model
perform action model =
    case action of
        Push dir ->
            push dir model

        Pop Up ->
            { model | above = List.drop 1 model.above }

        Pop Down ->
            { model | below = List.drop 1 model.below }

        Application function ->
            applied function model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updated =
            case msg of
                Do action ->
                    perform action model

                In str ->
                    if String.contains "--" str then
                        perform (Application Math.sub) model

                    else
                        { model | curr = Debug.log "in " str }

                Key key ->
                    case Dict.get key keymap of
                        Just action ->
                            perform action model

                        Nothing ->
                            model
    in
    ( updated, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
