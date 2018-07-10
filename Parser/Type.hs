module Parser.Type where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))



tVar :: Parser Type -- String 
tVar = do 
    fc  <- oneOf fch
    r   <- lexeme $ many $ oneOf rest
    mc <- optionMaybe $ lexeme $ string "->" 
    case mc of 
        Nothing -> return $ TVar ([fc] ++ r)
        Just t -> do 
            ec <- try $ type_ --sepBy type_ (lexeme $ string "->") 
            return $ TLambda (TVar ([fc] ++ r)) ec
    where fch = ['A'..'Z'] 
          rest =  fch ++ ['0'..'9'] ++ ['a'..'z'] ++ "_"

tVar0 :: Parser String-- String
tVar0 = do
    fc  <- oneOf fch
    r   <- lexeme $ many $ oneOf rest
    return $  ([fc] ++ r)
    where fch =  ['A'..'Z'] ++ ['a'..'z'] ++ "_"
          rest = fch ++ ['0'..'9']

tTuple:: Parser Type -- Type Type [Type] --8
tTuple = do 
    lb <- lexeme $ char '(' 
    first <- lexeme $ type_
    cb1 <- lexeme $ char ',' 
    second <- lexeme $ type_
    cb2 <- optionMaybe $ lexeme $ char ',' 
    case cb2 of 
        Nothing -> do 
            rb1 <- lexeme $ char ')' 
            return $ TTuple first second []
        Just dot -> do 
            rest <- try $ sepBy type_ (lexeme $ char ',') 
            rb2 <- lexeme $ char ')' 
            return $ TTuple first second rest

tdataTypecell :: Parser (String,Type)
tdataTypecell = do 
    c <- lexeme $ tVar0
    eq <- lexeme $ char ':' 
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