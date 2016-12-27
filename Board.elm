module Board exposing (..)

import Types exposing (..)
import Array exposing (..)


newRow : Row
newRow =
    List.repeat 3 N


newBoard : Board
newBoard =
    List.repeat 3 newRow


convertBoard : Board -> ArrayBoard
convertBoard =
    Array.fromList
        >> Array.map Array.fromList


getValFromRow : Int -> ArrayRow -> Turn
getValFromRow ind =
    Array.get ind >> Maybe.withDefault N


getMarker : Position -> ArrayBoard -> Turn
getMarker position =
    getCurrentRow position.y
        >> getValFromRow position.x


getCurrentRow : Int -> ArrayBoard -> Array Turn
getCurrentRow rowInd =
    Array.get rowInd >> Maybe.withDefault (Array.fromList newRow)


getLDiagonal : ArrayBoard -> ArrayRow
getLDiagonal =
    Array.indexedMap (\i row -> getValFromRow i row)


reverseArray : Array a -> Array a
reverseArray =
    Array.foldr Array.push Array.empty


getRDiagonal : ArrayBoard -> ArrayRow
getRDiagonal =
    reverseArray >> Array.indexedMap (\i row -> getValFromRow i row)
