module Source.Parser exposing (dump, dumpCodeSnippet, expression, parse)

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
    | Unreachable
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
    | ExpectingIfTest IfType
    | ExpectingIfConsequent IfType
    | ExpectingTernaryAlternative
    | ExpectingTernaryConsequent
    | ExpectingIfOrBlock
    | ExpectingExpressionOrBlock
    | ExpectingArrayElementOrArrayEnd
    | ExpectingArrowFunctionParamsOrParenthesizedExpression
    | ExpectingCommaOrArrayEnd
    | ExpectingMultiCommentStart
    | ExpectingMultiCommentEnd
    | ExpectingNoLineTerminator NoLineTerminator
    | ExpectingNoSemicolon
    | ExpectingRightBinaryOperand BinaryOperator
    | LocatedAt ( Int, Int ) Problem


type IfType
    = If
    | ElseIf


type EscapeSequence
    = EscapeTodo


type NoLineTerminator
    = AfterReturn
    | BetweenParamsAndArrow


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
    | InWhile
    | InIf IfType
    | InElse
    | InFor
    | InBlock
    | InArrow
    | BeforeFunctionDeclaration
    | InFunctionDeclaration String
    | InExpressionStatement
    | InBinary
    | InString Delimiter
    | InMultiline
    | InArray


type alias Parser a =
    Parser.Advanced.Parser Context Problem a


type alias DeadEnd =
    Parser.Advanced.DeadEnd Context Problem


reserved : Set.Set String
reserved =
    Set.fromList [ "break", "case", "catch", "continue", "debugger", "default", "delete", "do", "else", "finally", "for", "function", "if", "in", "instanceof", "new", "return", "switch", "this", "throw", "try", "typeof", "var", "void", "while", "with", "class", "const", "enum", "export", "extends", "import", "super", "implements", "interface", "let", "package", "private", "protected", "public", "static", "yield", "null", "true", "false" ]


parse : String -> Result (List DeadEnd) (List Statement)
parse =
    run program


program : Parser (List Statement)
program =
    spaces |> andThen (\_ -> statements |. end ExpectingEnd)



-- Semicolons


noEmptyStatement : Parser ()
noEmptyStatement =
    succeed Tuple.pair
        |. spaces
        |= getPosition
        |= oneOf
            [ succeed True |. symbol ";" Unreachable
            , succeed False
            ]
        |> andThen
            (\( pos, hasSemi ) ->
                if hasSemi then
                    problem <| LocatedAt pos ExpectingNoSemicolon

                else
                    succeed ()
            )



-- Whitespace


type NewLineInfo
    = NoNewLine
    | FirstNewLineAt ( Int, Int )


spaces : Parser ()
spaces =
    succeed () |. spacesCheckLineTerminator


spacesCheckLineTerminator : Parser NewLineInfo
spacesCheckLineTerminator =
    loop ( 0, NoNewLine ) <|
        ifProgress <|
            oneOf
                [ succeed FirstNewLineAt |. lineComment |= getPosition
                , multilineCheckLineTerminator
                , succeed FirstNewLineAt |. symbol "\n" Unreachable |= getPosition
                , succeed FirstNewLineAt |. symbol "\u{000D}" Unreachable |= getPosition
                , succeed NoNewLine |. chompWhile (\c -> c == ' ' || c == '\t')
                ]


lineComment : Parser ()
lineComment =
    symbol "//" Unreachable |. lineCommentHelper


lineCommentHelper : Parser ()
lineCommentHelper =
    oneOf
        [ end Unreachable
        , symbol "\n" Unreachable
        , chompIf (\_ -> True) Unreachable |. lazy (\_ -> lineCommentHelper)
        ]


ifProgress : Parser NewLineInfo -> ( Int, NewLineInfo ) -> Parser (Step ( Int, NewLineInfo ) NewLineInfo)
ifProgress parser ( offset, prevNewLineInfo ) =
    parser
        |> andThen
            (\newLineInfo ->
                succeed
                    (\newOffset ->
                        if offset == newOffset then
                            Done prevNewLineInfo

                        else
                            Loop
                                ( newOffset
                                , case prevNewLineInfo of
                                    NoNewLine ->
                                        newLineInfo

                                    FirstNewLineAt pos ->
                                        FirstNewLineAt pos
                                )
                    )
                    |= getOffset
            )


noLineTerminator : NoLineTerminator -> Parser ()
noLineTerminator location =
    spacesCheckLineTerminator
        |> andThen
            (\newLineInfo ->
                case newLineInfo of
                    NoNewLine ->
                        succeed ()

                    FirstNewLineAt pos ->
                        problem <| LocatedAt pos <| ExpectingNoLineTerminator location
            )


multilineCheckLineTerminator : Parser NewLineInfo
multilineCheckLineTerminator =
    symbol "/*" ExpectingMultiCommentStart |> andThen (\_ -> multilineCheckLineTerminatorHelper NoNewLine) |> inContext InMultiline


multilineCheckLineTerminatorHelper : NewLineInfo -> Parser NewLineInfo
multilineCheckLineTerminatorHelper newLineInfo =
    expecting ExpectingMultiCommentEnd <|
        oneOf
            [ succeed newLineInfo |. symbol "*/" Unreachable
            , chompIf (\c -> c /= '\n') Unreachable |> andThen (\_ -> multilineCheckLineTerminatorHelper newLineInfo)
            , chompIf (\c -> c == '\n') Unreachable
                |> andThen
                    (\_ ->
                        case newLineInfo of
                            NoNewLine ->
                                succeed FirstNewLineAt |= getPosition |. multilineCheckLineTerminatorHelper NoNewLine

                            FirstNewLineAt pos ->
                                multilineCheckLineTerminatorHelper (FirstNewLineAt pos)
                    )
            ]


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
                |. noLineTerminator AfterReturn
                |= expecting ExpectingExpression expression
                |. spaces
                |. symbol ";" (ExpectingSemi EndOfReturn)
        , (succeed identity |. keyword "function" |. spaces |= identifier)
            |> andThen
                (\id ->
                    inContext (InFunctionDeclaration id) <|
                        succeed (\params body -> Declaration Const id <| ArrowFunctionExpression params <| ArrowBodyBlock body)
                            |. spaces
                            |. symbol "(" (ExpectingLParen ParensParams)
                            |. spaces
                            |= closingNames
                            |. spaces
                            |= block
                            |. noEmptyStatement
                )
            |> inContext BeforeFunctionDeclaration
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
        , inContext InBlock <|
            succeed BlockStatement
                |= block
                |. noEmptyStatement
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
            |. noEmptyStatement
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
            |. noEmptyStatement
        , ifStatement If
        , inContext InExpressionStatement <|
            succeed ExpressionStatement
                |= expression
                |. spaces
                |. symbol ";" (ExpectingSemi EndOfExpressionStatement)

        --, expecting ExpectingStatement
        ]


ifStatement ifType =
    inContext (InIf ifType) <|
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
            |= expecting
                (ExpectingIfTest ifType)
                expression
            |. spaces
            |. symbol ")" (ExpectingRParen ParensIf)
            |. spaces
            |= expecting
                (ExpectingIfConsequent ifType)
                (inContext InBlock (map BlockStatement block))
            |. spaces
            |. keyword "else"
            |. spaces
            |= expecting ExpectingIfOrBlock
                (oneOf
                    [ lazy <| \_ -> ifStatement ElseIf
                    , inContext InBlock <| map BlockStatement block
                    ]
                )
            |. noEmptyStatement


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


problemIfNewLineBeforeArrow : NewLineInfo -> Expression -> Parser Expression
problemIfNewLineBeforeArrow newLineInfo expr =
    case ( newLineInfo, expr ) of
        ( NoNewLine, _ ) ->
            succeed expr

        ( FirstNewLineAt pos, ArrowFunctionExpression _ _ ) ->
            problem <| LocatedAt pos <| ExpectingNoLineTerminator BetweenParamsAndArrow

        ( FirstNewLineAt _, _ ) ->
            succeed expr


optionalArrow : Expression -> List String -> Parser Expression
optionalArrow exprIfNoArrow paramsIfArrow =
    spacesCheckLineTerminator
        |> andThen
            (\newLineInfo ->
                oneOf
                    [ inContext InArrow <| arrow paramsIfArrow
                    , succeed exprIfNoArrow
                    ]
                    |> andThen (problemIfNewLineBeforeArrow newLineInfo)
            )


arrow : List String -> Parser Expression
arrow paramsIfArrow =
    succeed identity
        |. symbol "=>" ExpectingArrow
        |. spaces
        |= expecting ExpectingExpressionOrBlock
            (oneOf
                [ succeed (\expr -> ArrowFunctionExpression paramsIfArrow <| ArrowBodyExpression expr) |= lazy (\_ -> expression)
                , succeed (\ss -> ArrowFunctionExpression paramsIfArrow <| ArrowBodyBlock ss) |= block
                ]
            )


parenthesizedExpressionOrArrowFunction : Parser Expression
parenthesizedExpressionOrArrowFunction =
    succeed identity
        |. symbol "(" (ExpectingLParen ParensGroupingOrArrowParams)
        |. spaces
        |= expecting ExpectingArrowFunctionParamsOrParenthesizedExpression
            (oneOf
                [ succeed Tuple.pair
                    |= identifier
                    |= spacesCheckLineTerminator
                    |> andThen
                        (\( id, newLineInfo ) ->
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
                                |> andThen (problemIfNewLineBeforeArrow newLineInfo)
                        )
                , succeed identity |. symbol ")" (ExpectingRParen ParensParams) |. noLineTerminator BetweenParamsAndArrow |= arrow []
                , lazy (\_ -> expression) |. symbol ")" (ExpectingRParen ParensGrouping)
                ]
            )


closingNames : Parser (List String)
closingNames =
    sequence
        { start = Token "" Unreachable
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
    inContext InArray <|
        succeed JsArray
            |. symbol "[" ExpectingArrayStart
            |. spaces
            |= expecting ExpectingArrayElementOrArrayEnd
                (oneOf
                    [ lazy (\_ -> expression |> andThen (\e -> loop [ e ] arrayHelp))
                    , symbol "]" ExpectingArrayStart |> map (\_ -> [])
                    ]
                )


arrayHelp revElements =
    succeed identity
        |. spaces
        |= expecting ExpectingCommaOrArrayEnd
            (oneOf
                [ succeed (\e -> Loop <| e :: revElements)
                    |. symbol "," Unreachable
                    |. spaces
                    |= expecting ExpectingExpression expression
                , succeed (Done <| List.reverse revElements) |. symbol "]" ExpectingArrayEnd
                ]
            )


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
    succeed (Number 1)
        |. oneOf
            [ symbol "0" Unreachable
            ]


string : Parser Expression
string =
    inContext (InString DoubleQuote) <|
        succeed JsString
            |. symbol "\"" (ExpectingStringStart DoubleQuote)
            |= loop [] stringHelp


specialChar : String -> Parser ()
specialChar c =
    symbol c (ExpectingSpecialChar c)


stringHelp : List String -> Parser (Step (List String) String)
stringHelp revChunks =
    expecting (ExpectingStringEnd DoubleQuote) <|
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
                |> andThen
                    (\chunk ->
                        if chunk == "" then
                            problem (ExpectingStringEnd DoubleQuote)

                        else
                            succeed <| Loop (chunk :: revChunks)
                    )
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
    if 4 > length && length > 6 then
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
    succeed identity
        |. spaces
        |= oneOf
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
            [ binaryOperators
                |> andThen
                    (\op ->
                        succeed identity
                            |. spaces
                            |= expecting (ExpectingRightBinaryOperand op) factor
                            |> andThen (\newExpr -> expressionHelp (( expr, op ) :: revOps) newExpr)
                            |> inContext InBinary
                    )
            , succeed (\c a -> ConditionalExpression { test = expr, consequent = c, alternate = a })
                |. symbol "?" ExpectingTernaryLeft
                |. spaces
                |= expecting ExpectingTernaryConsequent (lazy (\_ -> expression))
                |. spaces
                |. symbol ":" ExpectingTernaryRight
                |. spaces
                |= expecting ExpectingTernaryAlternative (lazy (\_ -> expression))
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


parseBinOp op =
    succeed op |. symbol (binaryOperatorToString op) (ExpectingBinary op)


binaryOperatorToString binOp =
    case binOp of
        TimesOp ->
            "*"

        DivideOp ->
            "/"

        RemainderOp ->
            "%"

        PlusOp ->
            "+"

        MinusOp ->
            "-"

        EqualsOp ->
            "==="

        NotEqualsOp ->
            "!=="

        LessThanEqualOp ->
            "<="

        LessThanOp ->
            "<"

        GreaterThanEqualOp ->
            ">="

        GreaterThanOp ->
            ">"

        AssignmentOp ->
            "="

        OrOp ->
            "||"

        AndOp ->
            "&&"


binaryOperators : Parser BinaryOperator
binaryOperators =
    oneOf
        [ parseBinOp TimesOp
        , parseBinOp DivideOp
        , parseBinOp RemainderOp
        , parseBinOp PlusOp
        , parseBinOp MinusOp
        , parseBinOp EqualsOp
        , parseBinOp NotEqualsOp
        , parseBinOp LessThanEqualOp
        , parseBinOp LessThanOp
        , parseBinOp GreaterThanEqualOp
        , parseBinOp GreaterThanOp
        , parseBinOp AssignmentOp
        , parseBinOp OrOp
        , parseBinOp AndOp
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


message : DeadEnd -> String
message err =
    (String.fromInt err.row ++ ":" ++ String.fromInt err.col)
        ++ (": " ++ problemToString err.problem)


contextToString : Context -> String
contextToString context =
    case context of
        InArray ->
            "array"

        InMultiline ->
            "multiline comment"

        InString DoubleQuote ->
            "string starting with `\"`"

        BeforeFunctionDeclaration ->
            "function declaration"

        InFunctionDeclaration id ->
            "function declaration of `" ++ id ++ "`"

        InArrow ->
            "lambda expression body"

        InReturn ->
            "return statement"

        InExpressionStatement ->
            "expression statement"

        InBinary ->
            "binary expression"

        InIf If ->
            "if statement"

        InIf ElseIf ->
            "else if statement"

        InBlock ->
            "block statement"

        _ ->
            Debug.toString context


problemToString : Problem -> String
problemToString problem =
    case problem of
        ExpectingArrayElementOrArrayEnd ->
            "Expecting first array element or `]`"

        ExpectingCommaOrArrayEnd ->
            "Expecting `,` or `]`"

        ExpectingEnd ->
            "Expecting another statement or end of program"

        ExpectingMultiCommentEnd ->
            "Expecting the closing `*/`"

        ExpectingIdentifier ->
            "Expecting a name"

        ExpectingExpressionOrBlock ->
            "Expecting an expression or a block statement"

        ExpectingKeyword k ->
            "Expecting `" ++ k ++ "`"

        ExpectingNoLineTerminator AfterReturn ->
            "Line breaks are not allowed after `return`"

        ExpectingNoLineTerminator BetweenParamsAndArrow ->
            "Line breaks are not allowed between the lambda expression parameters and the `=>`"

        ExpectingSemi _ ->
            "A `;` is missing"

        ExpectingIfTest _ ->
            "Expecting the test condition"

        ExpectingRightBinaryOperand op ->
            "Expecting an expression on the right of the operator `" ++ binaryOperatorToString op ++ "` operator"

        ExpectingNoSemicolon ->
            "The `;` is not allowed"

        LocatedAt _ p ->
            problemToString p

        ExpectingLParen _ ->
            "Expecting a `(`"

        ExpectingRParen _ ->
            "Expecting a `)`"

        ExpectingIfConsequent _ ->
            "Expecting the consequent block"

        ExpectingBlockStart ->
            "Expecting a `{`"

        ExpectingBlockEnd ->
            "Expecting a `}`"

        ExpectingIfOrBlock ->
            "Expecting `else if` or the else block"

        ExpectingStringEnd DoubleQuote ->
            "Expecting the closing `\"`"

        ExpectingExpression ->
            "Expecting an expression"

        _ ->
            Debug.toString problem


simplifyContexts : List { a | context : Context, row : b, col : c } -> List { a | context : Context, row : b, col : c }
simplifyContexts contexts =
    case contexts of
        [] ->
            []

        _ :: [] ->
            contexts

        a :: b :: rest ->
            case ( a.context, b.context ) of
                ( InIf ElseIf, InIf If ) ->
                    simplifyContexts <| a :: rest

                ( InIf ElseIf, InIf ElseIf ) ->
                    simplifyContexts <| a :: rest

                ( InFunctionDeclaration _, BeforeFunctionDeclaration ) ->
                    { a | row = b.row, col = b.col } :: simplifyContexts rest

                _ ->
                    a :: List.take 1 (simplifyContexts (b :: rest))


dump : String -> DeadEnd -> List String
dump input err =
    let
        simplifiedContextStack =
            simplifyContexts err.contextStack
    in
    ("Error:" ++ message err)
        :: List.map
            (\ctx ->
                ("  for the " ++ contextToString ctx.context ++ " starting at " ++ String.fromInt ctx.row)
                    ++ (":" ++ String.fromInt ctx.col)
            )
            simplifiedContextStack
        ++ ("" :: dumpCodeSnippet input { err | contextStack = simplifiedContextStack })


realLocation deadEnd =
    case deadEnd.problem of
        LocatedAt pos _ ->
            pos

        _ ->
            ( deadEnd.row, deadEnd.col )


dumpCodeSnippet : String -> DeadEnd -> List String
dumpCodeSnippet input deadEnd =
    let
        ( deadEndRow, deadEndCol ) =
            realLocation deadEnd

        range =
            case deadEnd.contextStack of
                [] ->
                    { startRow = deadEndRow
                    , startCol = deadEndCol
                    }

                ctx :: [] ->
                    { startRow = ctx.row
                    , startCol = ctx.col
                    }

                _ :: ctx :: _ ->
                    { startRow = ctx.row
                    , startCol = ctx.col
                    }

        strRows =
            { start = String.fromInt range.startRow
            , end = String.fromInt deadEndRow
            }

        lineNoWidth =
            Basics.max (String.length strRows.start) (String.length strRows.end)

        sourceSnippet =
            String.lines input
                |> List.drop (range.startRow - 1)
                |> List.take (deadEndRow - range.startRow + 1)
                |> List.indexedMap
                    (\i ln ->
                        String.padLeft lineNoWidth
                            ' '
                            (String.fromInt (range.startRow + i))
                            ++ ("|" ++ ln)
                    )

        underline =
            if List.isEmpty deadEnd.contextStack then
                String.repeat (lineNoWidth + deadEndCol) " " ++ "^"

            else if range.startRow == deadEndRow then
                String.repeat (lineNoWidth + range.startCol) " "
                    ++ (String.repeat (deadEndCol - range.startCol) "~" ++ "^")

            else
                String.repeat lineNoWidth " "
                    ++ ("+" ++ String.repeat (deadEndCol - 1) "~" ++ "^")
    in
    sourceSnippet ++ [ underline ]
