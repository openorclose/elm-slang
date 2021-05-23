module Main exposing (main)

import Array
import Browser
import Dict
import Html exposing (text)
import Html.Attributes
import Html.Events
import Source.Interpreter exposing (evaluateStatement, evaluateStatements)
import Source.Parser exposing (dumpCodeSnippet, expression, parse)


type alias Model =
    { input : String }


type Msg
    = Input String


main =
    Browser.element
        { init = \() -> ( { input = "" }, Cmd.none )
        , view =
            \m ->
                Html.div []
                    [ Html.textarea [ Html.Events.onInput Input, Html.Attributes.rows 20, Html.Attributes.cols 200 ] [ text m.input ]
                    , Html.pre []
                        [ Html.text <|
                            case parse m.input of
                                Ok ast ->
                                    Debug.toString ast

                                Err (e :: _) ->
                                    String.join "\n" <| Source.Parser.dump m.input e

                                Err _ ->
                                    ""
                        ]
                    , Html.div [] [ Html.text <| Debug.toString <| parse m.input ]
                    , Html.div [] [ Html.text <| Debug.toString <| Result.map Tuple.first <| (\s -> evaluateStatements s [ Dict.empty ] Array.empty) <| Result.withDefault [] <| parse m.input ]
                    ]
        , update = \(Input text) model -> ( { model | input = text }, Cmd.none )
        , subscriptions = always Sub.none
        }
