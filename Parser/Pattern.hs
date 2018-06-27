module Parser.Pattern where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))


pAnything :: Parser Pattern
pAnything = do 
    underscore <- lexeme $ char '_'
    return $ PAnything

pStr :: Parser Pattern--String
pStr = do
    fc    <- char '\"'
    mc  <- many $ oneOf anyChar
    ec   <- lexeme $ char '\"'
    return $ PStr mc
  where anyChar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++"_" ++ "-" ++ "." ++ " " 


----------------------------------------
decimalNumP :: Parser Pattern
decimalNumP = do
    fc  <- lexeme $ many1 $ oneOf numChar
    return $ PInt (read fc)
  where numChar = ['0'..'9']

hexNumP :: Parser Pattern
hexNumP = do
    fc  <- lexeme $ string "0X"
    rest  <- lexeme $ many1 $ oneOf numChar
    return $ PInt (read $ fc ++ rest)
  where numChar = ['0'..'9']


pInt :: Parser Pattern--Int
pInt = try hexNumP <|> decimalNumP


pVar :: Parser Pattern--String
pVar = do
    fc  <- oneOf fch
    r   <- lexeme $ many $ oneOf rest
    return $ PVar ([fc] ++ r)
    where fch = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
          rest =  fch ++ ['0'..'9']


pCons :: Parser Pattern--Pattern Pattern
pCons = do 
    fc <- try $ lexeme $ pattern0
    mc <- try $ lexeme $ char ':'
    ec <- try $ lexeme $ pattern
    return $ PCons fc ec

pList :: Parser Pattern--[Pattern]    -- ?
pList = do 
    lb <- lexeme $ char '[' 
    content <- try $ sepBy pattern (lexeme $ char ',') 
    rb <- lexeme $ char ']' 
    return $ PList content

pattern0:: Parser Pattern
pattern0  = try pAnything <|> pStr <|> pInt <|>  pVar 

pattern :: Parser Pattern
pattern = try pCons <|> pList  <|> pattern0


--patterns :: Parser [Pattern]
--patterns = spaces *> many (lexeme $ pattern)