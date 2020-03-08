module Main exposing (main)

import Browser exposing (Document)
import Debug
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as Elbg
import Element.Input as Elin
import Html exposing (Html)


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
    | In String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cells = Dict.empty, pos = 0 }
    , Cmd.none
    )


viewCell : ( Int, Expr ) -> Element Msg
viewCell ( idx, expr ) =
    case expr of
        Num val ->
            El.text <| String.fromFloat val


view : Model -> Document Msg
view { cells, pos } =
    let
        input : Element Msg
        input =
            Elin.text []
                { label = Elin.labelHidden "input"
                , onChange = In
                , placeholder = Nothing
                , text = ""
                }

        cellsView : Element Msg
        cellsView =
            El.column
                [ Elbg.color (El.rgb 0 0.5 0.1)
                , El.width <| El.fillPortion 2
                , El.height El.fill
                , El.inFront input
                ]
            <|
                List.map viewCell <|
                    Dict.toList cells

        controlsView : Element Msg
        controlsView =
            El.wrappedRow
                [ Elbg.color (El.rgb 0 0.1 0.5)
                , El.width <| El.fillPortion 3
                , El.height El.fill
                , El.padding 10
                , El.spacing 8
                ]
                [ El.text "hello world"
                , El.text "hello world"
                , El.text "hello world"
                , El.text "hello world"
                , El.text "hello world"
                , El.text "hello world"
                , El.text "hello world"
                , El.text "hello world"
                ]

        root : Html Msg
        root =
            El.layout [ El.explain Debug.todo ] <| El.row [] [ cellsView, controlsView ]
    in
    { title = "hello world", body = [ root ] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Push str ->
            ( model, Cmd.none )

        In str ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
