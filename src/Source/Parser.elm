module Source.Parser exposing (expression, parse)

import Parser exposing (..)
import Set
import Source.AST exposing (..)


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\u{000D}' || c == '\t')


noLineTerminator : Parser ()
noLineTerminator =
    chompWhile (\c -> c == ' ' || c == '\t')


reserved : Set.Set String
reserved =
    Set.fromList [ "break", "case", "catch", "continue", "debugger", "default", "delete", "do", "else", "finally", "for", "function", "if", "in", "instanceof", "new", "return", "switch", "this", "throw", "try", "typeof", "var", "void", "while", "with", "class", "const", "enum", "export", "extends", "import", "super", "implements", "interface", "let", "package", "private", "protected", "public", "static", "yield", "null", "true", "false" ]


parse : String -> Result (List DeadEnd) (List Statement)
parse =
    run program


program : Parser (List Statement)
program =
    spaces |> andThen (\_ -> statements |. end)


statements : Parser (List Statement)
statements =
    loop [] statementsHelp


statementsHelp : List Statement -> Parser (Step (List Statement) (List Statement))
statementsHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= statement
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse revStmts))
        ]


block : Parser (List Statement)
block =
    succeed identity
        |. symbol "{"
        |. spaces
        |= lazy (\_ -> statements)
        |. symbol "}"


statement : Parser Statement
statement =
    oneOf
        [ succeed Break |. keyword "break" |. spaces |. symbol ";"
        , succeed Continue |. keyword "continue" |. spaces |. symbol ";"
        , succeed Return |. keyword "return" |. noLineTerminator |= expression |. spaces |. symbol ";"
        , succeed ExpressionStatement |= expression |. spaces |. symbol ";"
        , succeed (\id params body -> Declaration Const id (ArrowFunctionExpressionBlockBody params body))
            |. keyword "function"
            |. spaces
            |= identifier
            |. spaces
            |. symbol "("
            |. spaces
            |= closingNames
            |. spaces
            |= block
        , succeed Declaration
            |= oneOf
                [ succeed Const |. keyword "const"
                , succeed Let |. keyword "let"
                ]
            |. spaces
            |= identifier
            |. spaces
            |. symbol "="
            |. spaces
            |= expression
            |. symbol ";"
        , succeed BlockStatement
            |= block
        , succeed WhileStatement
            |. keyword "while"
            |. spaces
            |. symbol "("
            |. spaces
            |= expression
            |. spaces
            |. symbol ")"
            |. spaces
            |= block
        , succeed
            (\init test update body ->
                ForStatement
                    { init = init
                    , test = test
                    , update = update
                    , body = body
                    }
            )
            |. keyword "for"
            |. spaces
            |. symbol "("
            |. spaces
            |= expression
            |. spaces
            |. symbol ";"
            |. spaces
            |= expression
            |. spaces
            |. symbol ";"
            |. spaces
            |= expression
            |. spaces
            |. symbol ")"
            |. spaces
            |= block
        , succeed
            (\test consequent alternate ->
                IfStatement
                    { test = test
                    , consequent = consequent
                    , alternate = alternate
                    }
            )
            |. keyword "if"
            |. spaces
            |. symbol "("
            |. spaces
            |= expression
            |. spaces
            |. symbol ")"
            |. spaces
            |= block
            |. spaces
            |. keyword "else"
            |. spaces
            |= block
        ]


term : Parser Expression
term =
    oneOf
        [ null
        , boolean
        , number
        , string
        , identifierOrShortArrowExpression
        , unaryOperation
        , array
        , parenthesizedExpressionOrArrowFunction
        ]


unaryOperation =
    oneOf
        [ succeed (UnaryOperation BooleanNot) |. symbol "!" |. spaces |= lazy (\_ -> term)
        , succeed (UnaryOperation UnaryNegation) |. symbol "-" |. spaces |= lazy (\_ -> term)
        ]


identifierOrShortArrowExpression : Parser Expression
identifierOrShortArrowExpression =
    identifier |> andThen (\id -> optionalArrow (Identifier id) [ id ])


optionalArrow : Expression -> List String -> Parser Expression
optionalArrow exprIfNoArrow paramsIfArrow =
    succeed identity
        |. spaces
        |= oneOf
            [ arrow paramsIfArrow, succeed exprIfNoArrow ]


arrow : List String -> Parser Expression
arrow paramsIfArrow =
    succeed identity
        |. symbol "=>"
        |. spaces
        |= oneOf
            [ succeed (ArrowFunctionExpression paramsIfArrow) |= lazy (\_ -> expression)
            , succeed (ArrowFunctionExpressionBlockBody paramsIfArrow) |= block
            ]


parenthesizedExpressionOrArrowFunction : Parser Expression
parenthesizedExpressionOrArrowFunction =
    succeed identity
        |. symbol "("
        |. spaces
        |= oneOf
            [ succeed identity
                |= identifier
                |. spaces
                |> andThen
                    (\id ->
                        oneOf
                            [ succeed identity |. symbol ")" |= optionalArrow (Identifier id) [ id ]
                            , succeed identity |= optionalArrow (Identifier id) [ id ] |. symbol ")"
                            , loop (Identifier id) callandPropertyAccess |> andThen (\f -> expressionHelp [] f |. symbol ")")
                            , succeed identity
                                |. symbol ","
                                |. spaces
                                |= closingNames
                                |. spaces
                                |> andThen (\xs -> arrow (id :: xs))
                            ]
                    )
            , succeed identity |. symbol ")" |. spaces |= arrow []
            , lazy (\_ -> expression) |. symbol ")"
            ]


closingNames : Parser (List String)
closingNames =
    sequence
        { start = ""
        , end = ")"
        , item = identifier
        , trailing = Forbidden
        , separator = ","
        , spaces = spaces
        }


callArgs : Parser (List Expression)
callArgs =
    succeed identity
        |= sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = spaces
            , item = lazy (\_ -> expression)
            , trailing = Forbidden
            }


array : Parser Expression
array =
    succeed JsArray
        |= sequence
            { start = "["
            , separator = ","
            , end = "]"
            , spaces = spaces
            , item = lazy (\_ -> expression)
            , trailing = Forbidden
            }


identifier : Parser String
identifier =
    succeed identity
        |= variable
            { start = \c -> Char.isAlpha c || List.any ((==) c) [ '_', '$' ]
            , inner = \c -> Char.isAlphaNum c || List.any ((==) c) [ '_', '$' ]
            , reserved = reserved
            }


null : Parser Expression
null =
    succeed Null |. symbol "null"


boolean : Parser Expression
boolean =
    oneOf
        [ succeed (Boolean True) |. symbol "true"
        , succeed (Boolean False) |. symbol "false"
        ]


number : Parser Expression
number =
    succeed Number
        |= Parser.float


string : Parser Expression
string =
    succeed JsString
        |. token "\""
        |= loop [] stringHelp


stringHelp : List String -> Parser (Step (List String) String)
stringHelp revChunks =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: revChunks))
            |. token "\\"
            |= oneOf
                [ map (\_ -> "\n") (token "n")
                , map (\_ -> "\t") (token "t")
                , map (\_ -> "\u{000D}") (token "r")
                , succeed String.fromChar
                    |. token "u{"
                    |= unicode
                    |. token "}"
                ]
        , token "\""
            |> map (\_ -> Done (String.join "" (List.reverse revChunks)))
        , chompWhile isUninteresting
            |> getChompedString
            |> map (\chunk -> Loop (chunk :: revChunks))
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'



-- UNICODE


unicode : Parser Char
unicode =
    getChompedString (chompWhile Char.isHexDigit)
        |> andThen codeToChar


codeToChar : String -> Parser Char
codeToChar str =
    let
        length =
            String.length str

        code =
            String.foldl addHex 0 str
    in
    if 4 <= length && length <= 6 then
        problem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        succeed (Char.fromCode code)

    else
        problem "code point must be between 0 and 0x10FFFF"


addHex : Char -> Int -> Int
addHex char total =
    let
        code =
            Char.toCode char
    in
    if 0x30 <= code && code <= 0x39 then
        16 * total + (code - 0x30)

    else if 0x41 <= code && code <= 0x46 then
        16 * total + (10 + code - 0x41)

    else
        16 * total + (10 + code - 0x61)


factor : Parser Expression
factor =
    succeed identity
        |= oneOf
            [ succeed (UnaryOperation BooleanNot) |. symbol "!" |. spaces |= lazy (\_ -> factor)
            , succeed (UnaryOperation UnaryNegation) |. symbol "-" |. spaces |= lazy (\_ -> factor)
            , term |> andThen (\t -> loop t callandPropertyAccess)
            ]


callandPropertyAccess expr =
    oneOf
        [ map (\args -> Loop <| Call expr args) callArgs
        , succeed (\property -> Loop <| PropertyAccess { object = expr, property = property })
            |. symbol "["
            |= lazy (\_ -> expression)
            |. symbol "]"
        , map (\_ -> Done expr) (succeed ())
        ]


expression : Parser Expression
expression =
    factor
        |> andThen (expressionHelp [])


expressionHelp : List ( Expression, BinaryOperator ) -> Expression -> Parser Expression
expressionHelp revOps expr =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed Tuple.pair
                |= operator
                |. spaces
                |= factor
                |> andThen (\( op, newExpr ) -> expressionHelp (( expr, op ) :: revOps) newExpr)
            , lazy (\_ -> succeed (finalize revOps [] [ expr ]))
            ]


isLowerPrecedence left right =
    isLowerPrecedenceHelp left right precedences


isLowerPrecedenceHelp left right xs =
    case xs of
        [] ->
            isLowerPrecedenceHelp left right []

        head :: tail ->
            if List.any ((==) left) head then
                False

            else if List.any ((==) right) head then
                True

            else
                isLowerPrecedenceHelp left right tail


precedences =
    [ [ TimesOp, DivideOp, RemainderOp ]
    , [ PlusOp, MinusOp ]
    , [ LessThanOp, LessThanEqualOp, GreaterThanOp, GreaterThanEqualOp ]
    , [ EqualsOp, NotEqualsOp ]
    , [ AndOp ]
    , [ OrOp ]
    , [ TernaryLeftOp, TernaryRightOp ]
    , [ AssignmentOp ]
    ]


operator : Parser BinaryOperator
operator =
    oneOf
        [ succeed TimesOp |. symbol "*"
        , succeed DivideOp |. symbol "/"
        , succeed RemainderOp |. symbol "%"
        , succeed PlusOp |. symbol "+"
        , succeed MinusOp |. symbol "-"
        , succeed EqualsOp |. symbol "==="
        , succeed NotEqualsOp |. symbol "!=="
        , succeed LessThanEqualOp |. symbol "<="
        , succeed LessThanOp |. symbol "<"
        , succeed GreaterThanEqualOp |. symbol ">="
        , succeed GreaterThanOp |. symbol ">"
        , succeed AssignmentOp |. symbol "="
        , succeed OrOp |. symbol "||"
        , succeed AndOp |. symbol "&&"
        , succeed TernaryLeftOp |. symbol "?"
        , succeed TernaryRightOp |. symbol ":"
        ]


finalize : List ( Expression, BinaryOperator ) -> List BinaryOperator -> List Expression -> Expression
finalize xs opStack stack =
    case ( xs, opStack, stack ) of
        ( [], op :: ops, left :: right :: exprs ) ->
            finalize [] ops (BinaryOperation { operator = op, left = left, right = right } :: exprs)

        ( [], _, [ head ] ) ->
            head

        ( ( expr, op ) :: rest, [], exprs ) ->
            finalize rest [ op ] (expr :: exprs)

        ( (( expr, op ) :: rest) as prevs, nextOp :: restOps, left :: right :: exprs ) ->
            if isLowerPrecedence op nextOp then
                finalize prevs
                    restOps
                    (BinaryOperation
                        { operator = nextOp
                        , left = left
                        , right = right
                        }
                        :: exprs
                    )

            else
                finalize rest (op :: nextOp :: restOps) (expr :: left :: right :: exprs)

        _ ->
            Debug.todo "cases"
