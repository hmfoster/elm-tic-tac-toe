module Types exposing (..)

import Array exposing (..)


type Turn
    = X
    | O
    | N


type alias Position =
    { x : Int, y : Int }


type alias Board =
    List Row


type alias Row =
    List Turn


type alias ArrayBoard =
    Array ArrayRow


type alias ArrayRow =
    Array Turn


type alias Model =
    --change this to a Union type
    { turn : Turn
    , board : Board
    , winner : Bool
    , numTurns : Int
    }
