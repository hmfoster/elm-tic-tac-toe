module Main exposing (..)

import Html exposing (..)
import Html.Events as E exposing (onClick)
import TakeTurn exposing (takeTurn)
import Types exposing (..)
import Board exposing (newBoard)
import Svg exposing (Svg, svg, rect, text_)
import Svg.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


init : ( Model, Cmd Msg )
init =
    ( Model X newBoard False 0, Cmd.none )



-- Update


type Msg
    = PlaceMarker Position
    | NewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PlaceMarker position ->
            if model.winner then
                ( model, Cmd.none )
            else
                ( takeTurn model position, Cmd.none )

        NewGame ->
            ( Model X newBoard False 0, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [] [ getMessage model ]
        , boardView model.board
        , button [ onClick NewGame ] [ text "New Game!" ]
        ]


getMessage : Model -> Html Msg
getMessage model =
    let
        winner =
            if model.turn == X then
                O
            else
                X
    in
        if model.winner then
            h2 [] [ text ((toString winner) ++ " Wins!") ]
        else if model.numTurns == 9 then
            h2 [] [ text ("It's a Draw!") ]
        else
            h2 [] [ text ("Go " ++ toString model.turn) ]


boardView : Board -> Html Msg
boardView board =
    let
        indexedBoard =
            List.indexedMap (,) board
    in
        div []
            [ table []
                [ tbody []
                    (List.map row indexedBoard)
                ]
            ]


row : ( Int, Row ) -> Html Msg
row ( y, row ) =
    let
        getPosition x marker =
            ( { x = x, y = y }, marker )

        positionedRow =
            List.indexedMap getPosition row
    in
        tr []
            (List.map space positionedRow)


space : ( Position, Turn ) -> Html Msg
space ( position, markerType ) =
    td [ E.onClick (PlaceMarker position) ]
        [ Svg.svg [ height "50", width "50" ]
            [ piece
            , text_ [ x "10", y "40", fontSize "50" ]
                [ text (marker markerType) ]
            ]
        ]


piece : Html Msg
piece =
    rect
        [ width "50"
        , height "50"
        , fill "white"
        , stroke "black"
        , strokeWidth "5"
        ]
        []


marker : Turn -> String
marker markerType =
    case markerType of
        X ->
            "X"

        O ->
            "O"

        N ->
            ""



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
