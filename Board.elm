module Board exposing (..)

import Types exposing (..)
import Array exposing (..)


newBoard : Board
newBoard =
    List.repeat 3 (List.repeat 3 N)


convertBoard : Board -> ArrayBoard
convertBoard board =
    Array.map Array.fromList (Array.fromList board)


getCurrentRow : ArrayBoard -> Position -> Array Turn
getCurrentRow board position =
    let
        arrayRow =
            Array.get position.y board

        defaultRow =
            Array.repeat 3 N
    in
        Maybe.withDefault defaultRow arrayRow


getLDiagonal : ArrayBoard -> ArrayRow
getLDiagonal board =
    let
        a =
            getMarker board { x = 0, y = 0 }

        b =
            getMarker board { x = 1, y = 1 }

        c =
            getMarker board { x = 2, y = 2 }
    in
        Array.fromList [ a, b, c ]


getRDiagonal : ArrayBoard -> ArrayRow
getRDiagonal board =
    let
        a =
            getMarker board { x = 0, y = 2 }

        b =
            getMarker board { x = 1, y = 1 }

        c =
            getMarker board { x = 2, y = 0 }
    in
        Array.fromList [ a, b, c ]


getValFromRow : ArrayRow -> Int -> Turn
getValFromRow row ind =
    Maybe.withDefault N (Array.get ind row)


getMarker : ArrayBoard -> Position -> Turn
getMarker board position =
    let
        row =
            getCurrentRow board position
    in
        getValFromRow row position.x
