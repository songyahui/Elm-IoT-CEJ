module Parser.AST where
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))

-- EXPRESSIONS
data Expr
  = Str String
  | Int Int
  | Float Double --1
  | Boolean Bool -- 2
  | Var String
  | List [Expr]
  | Negate Expr 
  | Binops String Expr Expr
  | Lambda [Pattern] Expr
  | Call Expr [Expr]
  | If [(Expr, Expr)] Expr
  | Let [Def] Expr
  | Case Expr [(Pattern, Expr)]
  | Tupple Expr Expr [Expr] -- 3
  | Record [(String, Expr)]
  | Update String [(String, Expr)]
  deriving (Show, Eq)

-- DEFINITIONS
data Def = Define (String) [Pattern] Expr deriving (Show, Eq)

-- PATTERN
data Pattern
  = PAnything
  | PStr String
  | PInt Int
  | PVar String 
  | PCons Pattern Pattern
  | PList [Pattern]   
  | PRecord [String]  --4
  | PTuple Pattern Pattern [Pattern] --5
  deriving (Show, Eq)

-- TYPE
data Type
  = TLambda Type Type  --6
  | TVar String 
  | TType String [Type] --7
  | TTuple Type Type [Type] --8
  | TRecord [(String, Type)] 
  deriving (Show, Eq)

-- DECLARATIONS
data Decl
  = Union String [String] [(String, [Type])]
  | Definition String [Pattern] Expr
  deriving (Show, Eq)


lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p <* spaces
           return x

lowVar :: Parser String-- String
lowVar = do
    fc  <- oneOf fch
    r   <- lexeme $ many $ oneOf rest
    return $ ([fc] ++ r)
    where fch =  ['a'..'z'] ++ "_"
          rest =  fch ++ ['0'..'9'] ++ ['A'..'Z'] 

uppVar :: Parser String-- String
uppVar = do
    fc  <- oneOf fch
    r   <- lexeme $ many $ oneOf rest
    return $ ([fc] ++ r)
    where fch =   ['A'..'Z'] ++ "_"
          rest =  fch ++ ['0'..'9'] ++ ['a'..'z'] 


--ssh pi@172.25.101.8
--sudo shutdown -h now