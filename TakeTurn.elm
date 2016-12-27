module TakeTurn exposing (takeTurn, checkForWinner)

import Board
    exposing
        ( convertBoard
        , getCurrentRow
        , getValFromRow
        , getLDiagonal
        , getRDiagonal
        )
import Array exposing (..)
import Maybe
import Types exposing (..)


takeTurn : Model -> Position -> Model
takeTurn model position =
    let
        currentRow =
            getCurrentRow position.y (convertBoard model.board)

        currentMarker =
            Maybe.withDefault N (Array.get position.x currentRow)

        marker =
            model.turn

        newTurn =
            switchTurn model.turn

        newBoard =
            placeMarker model.board position marker

        winner =
            model.winner || checkForWinner newBoard position marker
    in
        if currentMarker == N then
            Model newTurn newBoard winner (model.numTurns + 1)
        else
            model


switchTurn : Turn -> Turn
switchTurn currentTurn =
    case currentTurn of
        X ->
            O

        O ->
            X

        N ->
            X


placeMarker : Board -> Position -> Turn -> Board
placeMarker board position marker =
    let
        row =
            getCurrentRow position.y (convertBoard board)

        newRow =
            Array.set position.x marker row

        newBoard =
            Array.set position.y newRow (convertBoard board)
    in
        Array.toList (Array.map Array.toList newBoard)


checkForWinner : Board -> Position -> Turn -> Bool
checkForWinner board position marker =
    let
        convertedBoard =
            convertBoard board

        horizontal =
            checkHorizontal convertedBoard position marker

        vertical =
            checkVertical convertedBoard position marker

        diagonal =
            checkDiagonal convertedBoard marker
    in
        horizontal || vertical || diagonal


checkHorizontal : ArrayBoard -> Position -> Turn -> Bool
checkHorizontal board position marker =
    let
        row =
            getCurrentRow position.y board
    in
        isWinner row marker


checkVertical : ArrayBoard -> Position -> Turn -> Bool
checkVertical board position marker =
    let
        column =
            Array.map (\row -> getValFromRow position.x row) board
    in
        isWinner column marker


checkDiagonal : ArrayBoard -> Turn -> Bool
checkDiagonal board marker =
    (isWinner (getLDiagonal board) marker)
        || (isWinner (getRDiagonal board) marker)


isWinner : ArrayRow -> Turn -> Bool
isWinner values marker =
    Array.foldl (\m curr -> (m == marker) && curr) True values
