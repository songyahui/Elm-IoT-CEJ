module Generator.JSAST where
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))

data JSAST = JSAstProgram [JSState]
    deriving (Eq, Show)

data JSState
    = JSStateBlock [JSState] JSSemi-- ^lbrace, stmts, rbrace, autosemi
    | JSIf JSExpr JSState -- ^if,(,expr,),stmt
    | JSIfElse JSExpr JSState JSState -- ^if,(,expr,),stmt,else,rest
    | JSMethodCall JSExpr [JSExpr] JSSemi
    | JSReturn (Maybe JSExpr) JSSemi -- ^optional expression,autosemi
    | JSVariable [JSExpr] JSSemi -- ^var|const, decl, autosemi
    | JSWhile JSExpr JSState -- ^while,lb,expr,rb,stmt
    deriving ( Eq, Show)

data JSExpr
    -- | Terminals
    = JSIdentifier String
    | JSDecimal String
    | JSLiteral String --true false
    | JSHexInteger String
    | JSStringLiteral String
    | JSCallExprDot JSExpr JSExpr -- ^expr, dot, expr
    | JSExprBinary JSExpr JSBinOp JSExpr -- ^lhs, op, rhs
    | JSFunctionExpr String [String] [JSState] -- ^fn,name,lb, parameter list,rb,block`
    | JSMemberDot JSExpr JSExpr -- ^firstpart, dot, name
    | JSMemberExpr JSExpr [JSExpr] -- expr, lb, args, rb
    | JSMemberNew JSExpr [JSExpr] -- ^new, name, lb, args, rb
    | JSVarInitExpr JSExpr JSExpr -- ^identifier, initializer
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
