module Source.Interpreter exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Source.AST exposing (..)
import Source.Value as Value exposing (Frame, Value)


type alias Store =
    Array StoredValue


type StoredValue
    = Uninitialized
    | Constant Value
    | Variable Value


evaluateStatements : List Statement -> List Frame -> Store -> Result String ( Value, Store )
evaluateStatements statements frames store =
    let
        ids =
            statements
                |> List.filterMap
                    (\s ->
                        case s of
                            Declaration _ id _ ->
                                Just id

                            _ ->
                                Nothing
                    )

        blockFrame =
            List.indexedMap (\i p -> ( p, i + Array.length store )) ids |> Dict.fromList
    in
    helperStatements statements (blockFrame :: frames) (Array.append store (Array.fromList <| List.map (\_ -> Uninitialized) ids)) Value.Undefined


helperStatements : List Statement -> List Frame -> Store -> Value -> Result String ( Value, Store )
helperStatements statements frames store lastValue =
    case statements of
        [] ->
            Ok ( lastValue, store )

        s :: ss ->
            case evaluateStatement s frames store of
                Ok ( v, fs, nextStore ) ->
                    helperStatements ss fs nextStore v

                Err err ->
                    Err err


unsafeGet : Int -> Array a -> a
unsafeGet index arr =
    case Array.get index arr of
        Nothing ->
            unsafeGet index arr

        Just v ->
            v


evaluateStatement : Statement -> List Frame -> Store -> Result String ( Value, List Frame, Store )
evaluateStatement statement frames store =
    let
        insertFrames =
            Result.map (\( v, s ) -> ( v, frames, s ))
    in
    case statement of
        ExpressionStatement e ->
            insertFrames <| evaluateExpression e frames store

        Declaration t id e ->
            case evaluateExpression e frames store of
                Ok ( value, s ) ->
                    Ok
                        ( Value.Undefined
                        , frames
                        , Array.set (lookupRef id frames |> Maybe.withDefault -1)
                            (case t of
                                Const ->
                                    Constant value

                                Let ->
                                    Variable value
                            )
                            s
                        )

                Err err ->
                    Err err

        Return v ->
            case evaluateExpression v frames store of
                Ok ( value, s ) ->
                    Ok ( value, frames, s )

                Err err ->
                    Err err

        IfStatement { test, consequent, alternate } ->
            case evaluateExpression test frames store of
                Ok ( Value.Boolean True, nextStore ) ->
                    evaluateStatement consequent frames nextStore

                Ok ( Value.Boolean False, nextStore ) ->
                    evaluateStatement alternate frames nextStore

                Ok _ ->
                    Err "test condition must be bool"

                Err err ->
                    Err err

        _ ->
            Err "Unimplemented"


lookupRef : String -> List Frame -> Maybe Int
lookupRef id frames =
    case frames of
        [] ->
            Nothing

        frame :: rest ->
            case Dict.get id frame of
                Nothing ->
                    lookupRef id rest

                Just ref ->
                    Just ref


evaluateExpression : Expression -> List Frame -> Store -> Result String ( Value, Store )
evaluateExpression expression frames store =
    case expression of
        Number n ->
            Ok ( Value.Number n, store )

        Boolean bool ->
            Ok ( Value.Boolean bool, store )

        JsString string ->
            Ok ( Value.String string, store )

        Identifier string ->
            case
                lookupRef string frames
            of
                Just ref ->
                    case unsafeGet ref store of
                        Constant val ->
                            Ok ( val, store )

                        Variable val ->
                            Ok ( val, store )

                        Uninitialized ->
                            Err "Accessing var before decl"

                Nothing ->
                    Err "Undeclared name"

        Null ->
            Ok ( Value.Null, store )

        JsArray elements ->
            let
                helper remaining revFinal innerStore =
                    case remaining of
                        [] ->
                            Result.map (\xs -> ( Value.Array <| List.reverse xs, innerStore )) revFinal

                        x :: xs ->
                            case ( revFinal, evaluateExpression x frames innerStore ) of
                                ( _, Err err ) ->
                                    Err err

                                ( Err err, _ ) ->
                                    Err err

                                ( Ok otherRevFinals, Ok ( v, s ) ) ->
                                    helper xs (Ok (v :: otherRevFinals)) s
            in
            helper elements (Ok []) store

        Call callee args ->
            case evaluateExpression callee frames store of
                Ok ( Value.Function params ss closure, s ) ->
                    if List.length params /= List.length args then
                        Err "wrong number of args"

                    else
                        let
                            evalledArgs =
                                List.foldr
                                    (\arg acc ->
                                        case acc of
                                            Ok ( rest, prevStore ) ->
                                                case evaluateExpression arg frames prevStore of
                                                    Ok ( v, nextStore ) ->
                                                        Ok ( Variable v :: rest, nextStore )

                                                    Err err ->
                                                        Err err

                                            Err err ->
                                                Err err
                                    )
                                    (Ok ( [], s ))
                                    args
                        in
                        case evalledArgs of
                            Err err ->
                                Err err

                            Ok ( eas, newStore ) ->
                                let
                                    nextStore =
                                        Array.append newStore <| Array.fromList eas

                                    functionFrame =
                                        List.indexedMap (\i p -> ( p, i + Array.length newStore )) params |> Dict.fromList
                                in
                                evaluateStatements ss (functionFrame :: closure) nextStore

                Ok _ ->
                    Err "Not a function"

                Err err ->
                    Err err

        PropertyAccess { object, property } ->
            case evaluateExpression object frames store of
                Ok ( Value.Array arr, newStore ) ->
                    case evaluateExpression property frames newStore of
                        Ok ( Value.Number index, finalStore ) ->
                            if (index |> round |> toFloat) == index && index >= 0 then
                                case List.drop (round index) arr of
                                    [] ->
                                        Ok ( Value.Undefined, finalStore )

                                    x :: _ ->
                                        Ok ( x, finalStore )

                            else
                                Err "Array index must be int"

                        Ok ( _, _ ) ->
                            Err "array index must be int"

                        Err err ->
                            Err err

                Ok _ ->
                    Err "needs to be an array"

                Err err ->
                    Err err

        ArrowFunctionExpression strings body ->
            Ok
                ( Value.Function strings
                    (case body of
                        ArrowBodyBlock ss ->
                            ss

                        ArrowBodyExpression e ->
                            [ Return e ]
                    )
                    frames
                , store
                )

        UnaryOperation operator argument ->
            case evaluateExpression argument frames store of
                Ok ( evalledArg, nextStore ) ->
                    case ( operator, evalledArg ) of
                        ( UnaryNegation, Value.Number arg ) ->
                            Ok ( Value.Number -arg, nextStore )

                        ( UnaryNegation, _ ) ->
                            Err "unary - only takes in number as arg"

                        ( BooleanNot, Value.Boolean arg ) ->
                            Ok ( Value.Boolean <| not arg, nextStore )

                        ( BooleanNot, _ ) ->
                            Err "! only takes in boolean as arg"

                Err err ->
                    Err err

        BinaryOperation { operator, left, right } ->
            if operator == AssignmentOp then
                case left of
                    Identifier id ->
                        case lookupRef id frames of
                            Nothing ->
                                Err "undeclared name"

                            Just ref ->
                                case evaluateExpression right frames store of
                                    Ok ( value, nextStore ) ->
                                        case unsafeGet ref nextStore of
                                            Uninitialized ->
                                                Err "Cannot access before init"

                                            Constant _ ->
                                                Err "Cannot assign to constant"

                                            Variable _ ->
                                                Ok ( value, Array.set ref (Variable value) nextStore )

                                    Err err ->
                                        Err err

                    _ ->
                        Err "wrong LHS for ="

            else
                case evaluateExpression left frames store of
                    Ok ( leftValue, leftStore ) ->
                        case evaluateExpression right frames leftStore of
                            Ok ( rightValue, rightStore ) ->
                                Result.map (\r -> ( r, rightStore )) <| binaryOperatorFunction operator leftValue rightValue

                            Err err ->
                                Err err

                    Err err ->
                        Err err

        ConditionalExpression { test, consequent, alternate } ->
            case evaluateExpression test frames store of
                Ok ( Value.Boolean True, nextStore ) ->
                    evaluateExpression consequent frames nextStore

                Ok ( Value.Boolean False, nextStore ) ->
                    evaluateExpression alternate frames nextStore

                Ok _ ->
                    Err "test condition must be bool"

                Err err ->
                    Err err


binaryOperatorFunction : BinaryOperator -> (Value -> Value -> Result String Value)
binaryOperatorFunction op =
    case op of
        PlusOp ->
            acceptingNumbersOrString (\l r -> Value.Number <| l + r) (\l r -> Value.String <| l ++ r)

        MinusOp ->
            acceptingNumbers (\l r -> Value.Number <| l - r)

        TimesOp ->
            acceptingNumbers (\l r -> Value.Number <| l * r)

        RemainderOp ->
            acceptingNumbers (\l r -> Value.Number <| l - r * toFloat (floor (l / r)))

        DivideOp ->
            acceptingNumbers (\l r -> Value.Number <| l / r)

        LessThanOp ->
            acceptingNumbersOrString (\l r -> Value.Boolean <| l < r) (\l r -> Value.Boolean <| l < r)

        LessThanEqualOp ->
            acceptingNumbersOrString (\l r -> Value.Boolean <| l <= r) (\l r -> Value.Boolean <| l <= r)

        GreaterThanOp ->
            acceptingNumbersOrString (\l r -> Value.Boolean <| l > r) (\l r -> Value.Boolean <| l > r)

        GreaterThanEqualOp ->
            acceptingNumbersOrString (\l r -> Value.Boolean <| l >= r) (\l r -> Value.Boolean <| l >= r)

        EqualsOp ->
            \l r -> Ok <| Value.Boolean <| l == r

        NotEqualsOp ->
            \l r -> Ok <| Value.Boolean <| l /= r

        _ ->
            \_ _ -> Err "Unreachable"


acceptingNumbers : (Float -> Float -> Value) -> Value -> Value -> Result String Value
acceptingNumbers f left right =
    case ( left, right ) of
        ( Value.Number l, Value.Number r ) ->
            Ok <| f l r

        _ ->
            Err "expecting two numbers"


acceptingNumbersOrString : (Float -> Float -> Value) -> (String -> String -> Value) -> Value -> Value -> Result String Value
acceptingNumbersOrString fNumber fString left right =
    case ( left, right ) of
        ( Value.Number l, Value.Number r ) ->
            Ok <| fNumber l r

        ( Value.String l, Value.String r ) ->
            Ok <| fString l r

        _ ->
            Err "expecting two numbers or two strings"
