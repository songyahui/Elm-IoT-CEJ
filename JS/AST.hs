module AST where
import Data.Data
import Data.List





data JSAST = JSAstProgram [JSStatement]
    deriving (Eq, Show)

data JSStatement
    = JSStatementBlock  [JSStatement]  JSSemi     -- ^lbrace, stmts, rbrace, autosemi
    | JSIf   JSExpression  JSStatement -- ^if,(,expr,),stmt
    | JSIfElse   JSExpression  JSStatement  JSStatement -- ^if,(,expr,),stmt,else,rest
    | JSMethodCall JSExpression  [JSExpression]  JSSemi
    | JSReturn  (Maybe JSExpression) JSSemi -- ^optional expression,autosemi
    | JSVariable  [JSExpression] JSSemi -- ^var|const, decl, autosemi
    | JSWhile   JSExpression  JSStatement -- ^while,lb,expr,rb,stmt
    deriving ( Eq, Show)

data JSExpression
    -- | Terminals
    = JSIdentifier  String
    | JSDecimal  String
    | JSLiteral  String --true false
    | JSHexInteger  String
    | JSStringLiteral  String

    -- | Non Terminals
    | JSCallExpressionDot JSExpression  JSExpression  -- ^expr, dot, expr
    | JSExpressionBinary JSExpression JSBinOp JSExpression -- ^lhs, op, rhs
    | JSFunctionExpression  String  [String]  [JSStatement] -- ^fn,name,lb, parameter list,rb,block`
    | JSMemberDot JSExpression  JSExpression -- ^firstpart, dot, name
    | JSMemberExpression JSExpression  [JSExpression]  -- expr, lb, args, rb
    | JSMemberNew  JSExpression  [JSExpression]  -- ^new, name, lb, args, rb
    | JSVarInitExpression JSExpression JSExpression -- ^identifier, initializer
    deriving ( Eq, Show)

data JSBinOp
    = JSBinOpDivide  -- /
    | JSBinOpEq  -- =
    | JSBinOpGe  -- >=
    | JSBinOpGt  -- >
    | JSBinOpLe  -- <=
    | JSBinOpLt  -- <
    | JSBinOpMinus  -- -
    | JSBinOpNeq  -- =
    | JSBinOpPlus  -- +
    | JSBinOpTimes  -- *
    deriving ( Eq, Show)


data JSSemi = JSSemiAuto -- ;
    deriving ( Eq, Show)

data JSAssignOp = JSAssign 
    deriving ( Eq, Show)