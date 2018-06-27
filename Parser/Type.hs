module Parser.Type where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))



tVar :: Parser Type -- String 
tVar = do 
    fc  <- oneOf fch
    r   <- lexeme $ many $ oneOf rest
    return $ TVar ([fc] ++ r)
    where fch = ['A'..'Z'] ++ ['a'..'z']
          rest =  fch ++ ['0'..'9']  ++ "_"

tVar0 :: Parser String-- String
tVar0 = do
    fc  <- oneOf fch
    r   <- lexeme $ many $ oneOf rest
    return $  ([fc] ++ r)
    where fch =  ['A'..'Z'] ++ ['a'..'z'] ++ "_"
          rest = fch ++ ['0'..'9']



tdataTypecell :: Parser (String,Type)
tdataTypecell = do 
    c <- lexeme $ tVar0
    eq <- lexeme $ char '=' 
    ex <- lexeme $ type_
    return (c,ex)

tRecord :: Parser Type -- [(String, Type)] 
tRecord = do 
    fc <- lexeme $ char '{' 
    mc <- lexeme $ try $ sepBy tdataTypecell (lexeme $ char ',') 
    ec <- lexeme $ char '}'
    return $ TRecord mc


type_ :: Parser Type 
type_ = try tVar <|> tRecord

--types :: Parser [Type]
--types = spaces *> many (lexeme $ type_)