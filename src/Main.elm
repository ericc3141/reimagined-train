module Main exposing (main)

import Browser exposing (Document)
import Debug
import Dict exposing (Dict)
import Element as El exposing (Element)
import Element.Background as Elbg
import Element.Input as Elin
import Html exposing (Html)
import Html.Events as HtmlE
import Json.Decode as JsonD


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { above : List Expr
    , below : List Expr
    , curr : String
    }


type Expr
    = Num Float


type Msg
    = Push String
    | In String
    | Key String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { above = [ Num 1 ], below = [ Num -1 ], curr = "" }
    , Cmd.none
    )


viewCell : Expr -> Element Msg
viewCell expr =
    case expr of
        Num val ->
            El.el
                [ El.width <| El.px 300
                , Elbg.color (El.rgb 0 0.1 0.5)
                ]
            <|
                El.text <|
                    String.fromFloat val


view : Model -> Document Msg
view { above, below, curr } =
    let
        cellsView : Element Msg
        cellsView =
            Elin.text
                [ El.centerX
                , El.centerY
                , El.above <| El.column [] <| List.map viewCell above
                , El.below <| El.column [] <| List.map viewCell below
                ]
                { label = Elin.labelHidden "input"
                , onChange = In
                , placeholder = Nothing
                , text = curr
                }

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

        keyDown : El.Attribute Msg
        keyDown =
            El.htmlAttribute
                (HtmlE.on "keydown"
                    (JsonD.field "key"
                        (JsonD.string |> JsonD.map Key)
                    )
                )

        root : Html Msg
        root =
            El.layout [ keyDown ] <|
                El.row
                    [ El.height El.fill
                    , El.width El.fill
                    ]
                    [ cellsView, controlsView ]
    in
    { title = "hello world", body = [ root ] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Push str ->
            ( model, Cmd.none )

        In str ->
            ( { model | curr = str }, Cmd.none )

        Key str ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
