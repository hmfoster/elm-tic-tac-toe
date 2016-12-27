module Main exposing (..)

import Html exposing (..)
import Html.Events as E exposing (onClick)
import TakeTurn exposing (takeTurn)
import Types exposing (..)
import Board exposing (newBoard)
import Views exposing (..)


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
        [ div [] [ message model ]
        , boardView model.board
        , button [ onClick NewGame ] [ text "New Game!" ]
        ]



-- Subscription


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
