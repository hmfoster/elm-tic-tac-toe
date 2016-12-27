module Views exposing (..)

import Types exposing (..)
import Html exposing (..)
import Html.Events as E exposing (onClick)
import Svg exposing (Svg, svg, rect, text_)
import Svg.Attributes exposing (..)


message : Model -> Html Msg
message model =
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
