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
    ( Model X newBoard False, Cmd.none )



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
            ( Model X newBoard False, Cmd.none )



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
            (List.map marker positionedRow)


marker : ( Position, Turn ) -> Html Msg
marker ( position, markerType ) =
    td [ E.onClick (PlaceMarker position) ] [ piece markerType ]


piece : Turn -> Html Msg
piece markerType =
    case markerType of
        X ->
            Svg.svg [ height "50", width "50" ]
                [ rect [ width "50", height "50", fill "white", stroke "black", strokeWidth "5" ] []
                , text_ [ x "10", y "40", fontSize "50" ] [ text "X" ]
                ]

        O ->
            Svg.svg [ height "50", width "50" ]
                [ rect [ width "50", height "50", fill "white", stroke "black", strokeWidth "5" ] []
                , text_ [ x "10", y "40", fontSize "50" ] [ text "O" ]
                ]

        N ->
            Svg.svg [ height "50", width "50" ]
                [ rect [ width "50", height "50", fill "white", stroke "black", strokeWidth "5" ] []
                ]



-- td [ E.onClick (Debug.log "cat") ] [ text piece ]
-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
