module Source.Parser exposing (expression, parse)

import Parser.Advanced exposing (..)
import Set
import Source.AST exposing (..)


type Problem
    = ExpectingEnd
    | ExpectingBlockStart
    | ExpectingBlockEnd
    | ExpectingSemi SemiFor
    | ExpectingKeyword String
    | ExpectingLParen ParensFor
    | ExpectingRParen ParensFor
    | ExpectingArrayStart
    | ExpectingArrayEnd
    | ExpectingMemberStart
    | ExpectingMemberEnd
    | ExpectingArraySeparator
    | ExpectingParamsSeparator
    | ExpectingArgsSeparator
    | ExpectingIdentifier
    | ExpectingNumber
    | ExpectingDeclarationEquals
    | Never
    | ExpectingUnary UnaryOperator
    | ExpectingBinary BinaryOperator
    | ExpectingArrow
    | ExpectingStringStart Delimiter
    | ExpectingStringEnd Delimiter
    | ExpectingEscapeStart
    | ExpectingUnicodeStart
    | ExpectingUnicodeEnd
    | ExpectingSpecialChar String
    | UnicodeProblem String
    | ExpectingTernaryLeft
    | ExpectingTernaryRight
    | ExpectingExpression
    | ExpectingIfTest
    | ExpectingIfConsequent
    | ExpectingIfOrBlock
    | ExpectingExpressionOrBlock


type EscapeSequence
    = EscapeTodo


type Delimiter
    = SingleQuote
    | DoubleQuote
    | Backtick


type ParensFor
    = ParensParams
    | ParensIf
    | ParensWhile
    | ParensFor
    | ParensCall
    | ParensGrouping
    | ParensGroupingOrArrowParams


type SemiFor
    = EndOfBreak
    | EndOfContinue
    | EndOfReturn
    | EndOfExpressionStatement
    | EndOfDeclaration
    | EndForInit
    | EndForTest


type Context
    = InReturn
    | InDeclaration DeclarationType String


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Problem


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
    spaces |> andThen (\_ -> statements |. end ExpectingEnd)


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
        |. symbol "{" ExpectingBlockStart
        |. spaces
        |= lazy (\_ -> statements)
        |. symbol "}" ExpectingBlockEnd


symbol : String -> Problem -> Parser ()
symbol token problem =
    Parser.Advanced.symbol (Token token problem)


keyword : String -> Parser ()
keyword word =
    Parser.Advanced.keyword (Token word (ExpectingKeyword word))


statement : Parser Statement
statement =
    oneOf
        [ succeed Break
            |. keyword "break"
            |. spaces
            |. symbol ";" (ExpectingSemi EndOfBreak)
        , succeed Continue
            |. keyword "continue"
            |. spaces
            |. symbol ";" (ExpectingSemi EndOfContinue)
        , inContext InReturn <|
            succeed Return
                |. keyword "return"
                |. noLineTerminator
                |= expression
                |. spaces
                |. symbol ";" (ExpectingSemi EndOfReturn)
        , succeed (\id params body -> Declaration Const id (ArrowFunctionExpressionBlockBody params body))
            |. keyword "function"
            |. spaces
            |= identifier
            |. spaces
            |. symbol "(" (ExpectingLParen ParensParams)
            |. spaces
            |= closingNames
            |. spaces
            |= block
        , oneOf
            [ succeed Const |. keyword "const"
            , succeed Let |. keyword "let"
            ]
            |. spaces
            |> andThen
                (\t ->
                    identifier
                        |. spaces
                        |> andThen
                            (\id ->
                                inContext (InDeclaration t id) <|
                                    succeed (Declaration t id)
                                        |. symbol "=" ExpectingDeclarationEquals
                                        |. spaces
                                        |= expecting ExpectingExpression expression
                                        |. symbol ";" (ExpectingSemi EndOfDeclaration)
                            )
                )
        , succeed BlockStatement
            |= block
        , succeed WhileStatement
            |. keyword "while"
            |. spaces
            |. symbol "(" (ExpectingLParen ParensWhile)
            |. spaces
            |= expression
            |. spaces
            |. symbol ")" (ExpectingRParen ParensWhile)
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
            |. symbol "(" (ExpectingLParen ParensFor)
            |. spaces
            |= expression
            |. spaces
            |. symbol ";" (ExpectingSemi EndForInit)
            |. spaces
            |= expression
            |. spaces
            |. symbol ";" (ExpectingSemi EndForTest)
            |. spaces
            |= expression
            |. spaces
            |. symbol ")" (ExpectingRParen ParensFor)
            |. spaces
            |= block
        , ifStatement
        , succeed ExpressionStatement
            |= expression
            |. spaces
            |. symbol ";" (ExpectingSemi EndOfExpressionStatement)

        --, expecting ExpectingStatement
        ]


ifStatement =
    succeed
        (\test consequent alternate ->
            IfStatement
                { test = test
                , consequent = consequent
                , alternate = alternate
                }
        )
        |. keyword "if"
        |. spaces
        |. symbol "(" (ExpectingLParen ParensIf)
        |. spaces
        |= expecting ExpectingIfTest expression
        |. spaces
        |. symbol ")" (ExpectingRParen ParensIf)
        |. spaces
        |= expecting ExpectingIfConsequent (map BlockStatement block)
        |. spaces
        |. keyword "else"
        |. spaces
        |= expecting ExpectingIfOrBlock
            (oneOf
                [ lazy <| \_ -> ifStatement
                , map BlockStatement block
                ]
            )


expecting : Problem -> Parser a -> Parser a
expecting orElse toBe =
    oneOf
        [ toBe
        , commit () |> andThen (\_ -> problem orElse)
        ]


term : Parser Expression
term =
    oneOf
        [ null
        , boolean
        , number
        , string
        , identifierOrShortArrowExpression
        , array
        , parenthesizedExpressionOrArrowFunction
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
        |. symbol "=>" ExpectingArrow
        |. spaces
        |= expecting ExpectingExpressionOrBlock
            (oneOf
                [ succeed (ArrowFunctionExpression paramsIfArrow) |= lazy (\_ -> expression)
                , succeed (ArrowFunctionExpressionBlockBody paramsIfArrow) |= block
                ]
            )


parenthesizedExpressionOrArrowFunction : Parser Expression
parenthesizedExpressionOrArrowFunction =
    succeed identity
        |. symbol "(" (ExpectingLParen ParensGroupingOrArrowParams)
        |. spaces
        |= oneOf
            [ succeed identity
                |= identifier
                |. spaces
                |> andThen
                    (\id ->
                        oneOf
                            [ succeed identity |. symbol ")" (ExpectingRParen ParensGroupingOrArrowParams) |= optionalArrow (Identifier id) [ id ]
                            , succeed identity |= optionalArrow (Identifier id) [ id ] |. symbol ")" (ExpectingRParen ParensGrouping)
                            , loop (Identifier id) callandPropertyAccess |> andThen (\f -> expressionHelp [] f |. symbol ")" (ExpectingRParen ParensGrouping))
                            , succeed identity
                                |. symbol "," ExpectingArgsSeparator
                                |. spaces
                                |= closingNames
                                |. spaces
                                |> andThen (\xs -> arrow (id :: xs))
                            ]
                    )
            , succeed identity |. symbol ")" (ExpectingRParen ParensParams) |. spaces |= arrow []
            , lazy (\_ -> expression) |. symbol ")" (ExpectingRParen ParensGrouping)
            ]


closingNames : Parser (List String)
closingNames =
    sequence
        { start = Token "" Never
        , end = Token ")" (ExpectingRParen ParensParams)
        , item = identifier
        , trailing = Forbidden
        , separator = Token "," ExpectingParamsSeparator
        , spaces = spaces
        }


callArgs : Parser (List Expression)
callArgs =
    succeed identity
        |= sequence
            { start = Token "(" (ExpectingLParen ParensCall)
            , separator = Token "," ExpectingParamsSeparator
            , end = Token ")" (ExpectingRParen ParensCall)
            , spaces = spaces
            , item = expecting ExpectingExpression <| lazy (\_ -> expression)
            , trailing = Forbidden
            }


array : Parser Expression
array =
    succeed JsArray
        |= sequence
            { start = Token "[" ExpectingArrayStart
            , separator = Token "," ExpectingArraySeparator
            , end = Token "]" ExpectingArrayEnd
            , spaces = spaces
            , item = expecting ExpectingExpression <| lazy (\_ -> expression)
            , trailing = Forbidden
            }


identifier : Parser String
identifier =
    succeed identity
        |= variable
            { start = \c -> Char.isAlpha c || List.any ((==) c) [ '_', '$' ]
            , inner = \c -> Char.isAlphaNum c || List.any ((==) c) [ '_', '$' ]
            , reserved = reserved
            , expecting = ExpectingIdentifier
            }


null : Parser Expression
null =
    succeed Null |. keyword "null"


boolean : Parser Expression
boolean =
    oneOf
        [ succeed (Boolean True) |. keyword "true"
        , succeed (Boolean False) |. keyword "false"
        ]


number : Parser Expression
number =
    Parser.Advanced.number
        { int = Ok (toFloat >> Number)
        , hex = Ok (toFloat >> Number)
        , octal = Ok (toFloat >> Number)
        , binary = Ok (toFloat >> Number)
        , float = Ok Number
        , invalid = Never
        , expecting = ExpectingNumber
        }


string : Parser Expression
string =
    succeed JsString
        |. symbol "\"" (ExpectingStringStart DoubleQuote)
        |= loop [] stringHelp


specialChar : String -> Parser ()
specialChar c =
    symbol c (ExpectingSpecialChar c)


stringHelp : List String -> Parser (Step (List String) String)
stringHelp revChunks =
    oneOf
        [ succeed (\chunk -> Loop (chunk :: revChunks))
            |. symbol "\\" ExpectingEscapeStart
            |= oneOf
                [ map (\_ -> "\n") (specialChar "n")
                , map (\_ -> "\t") (specialChar "t")
                , map (\_ -> "\u{000D}") (specialChar "r")
                , succeed String.fromChar
                    |. symbol "u{" ExpectingUnicodeStart
                    |= unicode
                    |. symbol "}" ExpectingUnicodeEnd
                ]
        , symbol "\"" (ExpectingStringEnd DoubleQuote)
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
        problem <| UnicodeProblem "code point must have between 4 and 6 digits"

    else if 0 <= code && code <= 0x0010FFFF then
        succeed (Char.fromCode code)

    else
        problem <| UnicodeProblem "code point must be between 0 and 0x10FFFF"


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
            [ succeed (UnaryOperation BooleanNot) |. symbol "!" (ExpectingUnary BooleanNot) |. spaces |= lazy (\_ -> factor)
            , succeed (UnaryOperation UnaryNegation) |. symbol "-" (ExpectingUnary UnaryNegation) |. spaces |= lazy (\_ -> factor)
            , term |> andThen (\t -> loop t callandPropertyAccess)
            ]


callandPropertyAccess expr =
    oneOf
        [ map (\args -> Loop <| Call expr args) callArgs
        , succeed (\property -> Loop <| PropertyAccess { object = expr, property = property })
            |. symbol "[" ExpectingMemberStart
            |= lazy (\_ -> expression)
            |. symbol "]" ExpectingMemberEnd
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
            , succeed (\c a -> ConditionalExpression { test = expr, consequent = c, alternate = a })
                |. symbol "?" ExpectingTernaryLeft
                |. spaces
                |= lazy (\_ -> expression)
                |. spaces
                |. symbol ":" ExpectingTernaryRight
                |. spaces
                |= lazy (\_ -> expression)
            , lazy (\_ -> succeed (finalize revOps [] [ expr ]))
            ]


comparePrecedences left right =
    comparePrecedencesHelp left right precedences


comparePrecedencesHelp left right xs =
    case xs of
        [] ->
            EQ

        head :: tail ->
            case ( List.any ((==) left) head, List.any ((==) right) head ) of
                ( True, True ) ->
                    EQ

                ( False, True ) ->
                    LT

                ( True, False ) ->
                    GT

                ( False, False ) ->
                    comparePrecedencesHelp left right tail


type Associativity
    = Left
    | Right


rightAssociative =
    [ AndOp, OrOp ]


associativity op =
    if List.any ((==) op) rightAssociative then
        Right

    else
        Left


precedences =
    [ [ TimesOp, DivideOp, RemainderOp ]
    , [ PlusOp, MinusOp ]
    , [ LessThanOp, LessThanEqualOp, GreaterThanOp, GreaterThanEqualOp ]
    , [ EqualsOp, NotEqualsOp ]
    , [ AndOp ]
    , [ OrOp ]
    , [ AssignmentOp ]
    ]


parseBinOp constructor op =
    succeed constructor |. symbol op (ExpectingBinary constructor)


operator : Parser BinaryOperator
operator =
    oneOf
        [ parseBinOp TimesOp "*"
        , parseBinOp DivideOp "/"
        , parseBinOp RemainderOp "%"
        , parseBinOp PlusOp "+"
        , parseBinOp MinusOp "-"
        , parseBinOp EqualsOp "==="
        , parseBinOp NotEqualsOp "!=="
        , parseBinOp LessThanEqualOp "<="
        , parseBinOp LessThanOp "<"
        , parseBinOp GreaterThanEqualOp ">="
        , parseBinOp GreaterThanOp ">"
        , parseBinOp AssignmentOp "="
        , parseBinOp OrOp "||"
        , parseBinOp AndOp "&&"
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
            case comparePrecedences op nextOp of
                LT ->
                    finalize prevs
                        restOps
                        (BinaryOperation
                            { operator = nextOp
                            , left = left
                            , right = right
                            }
                            :: exprs
                        )

                EQ ->
                    case associativity op of
                        Left ->
                            finalize rest (op :: nextOp :: restOps) (expr :: left :: right :: exprs)

                        Right ->
                            finalize prevs
                                restOps
                                (BinaryOperation
                                    { operator = nextOp
                                    , left = left
                                    , right = right
                                    }
                                    :: exprs
                                )

                GT ->
                    finalize rest (op :: nextOp :: restOps) (expr :: left :: right :: exprs)

        _ ->
            Null
