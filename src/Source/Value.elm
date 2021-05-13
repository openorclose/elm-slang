module Source.Value exposing (..)

import Dict exposing (Dict)
import Source.AST exposing (Statement)


type Value
    = Undefined
    | Null
    | Number Float
    | Boolean Bool
    | String String
    | Array (List Value)
    | Function (List String) (List Statement) (List Frame)


type alias Frame =
    Dict String Int
