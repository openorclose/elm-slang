module Source.AST exposing (..)


type DeclarationType
    = Let
    | Const


type Statement
    = ExpressionStatement Expression
    | Declaration DeclarationType String Expression
    | BlockStatement (List Statement)
    | IfStatement { test : Expression, consequent : List Statement, alternate : List Statement }
    | WhileStatement Expression (List Statement)
    | ForStatement { init : Expression, test : Expression, update : Expression, body : List Statement }
    | Break
    | Continue
    | Return Expression


type Expression
    = Boolean Bool
    | Number Float
    | JsString String
    | Identifier String
    | Null
    | JsArray (List Expression)
    | Call Expression (List Expression)
    | PropertyAccess { object : Expression, property : Expression }
    | ArrowFunctionExpression (List String) Expression
    | ArrowFunctionExpressionBlockBody (List String) (List Statement)
    | UnaryOperation UnaryOperator Expression
    | BinaryOperation { operator : BinaryOperator, left : Expression, right : Expression }


type UnaryOperator
    = BooleanNot
    | UnaryNegation


type BinaryOperator
    = PlusOp
    | TimesOp
    | MinusOp
    | DivideOp
    | RemainderOp
    | AndOp
    | OrOp
    | AssignmentOp
    | TernaryLeftOp
    | TernaryRightOp
    | EqualsOp
    | NotEqualsOp
    | LessThanOp
    | LessThanEqualOp
    | GreaterThanOp
    | GreaterThanEqualOp
