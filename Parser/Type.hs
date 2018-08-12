module Parser.Type where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))



variable :: Parser Type -- String 
variable = do 
    fc  <- lexeme $ lowVar_Not_Key
    rest <- lexeme $ optionMaybe tLambda
    case rest of 
        Nothing -> return $ TVar fc
        Just rr -> return $ TLambda (TType fc []) rr

tuple:: Parser Type -- Type Type [Type] --8
tuple = do 
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
    c <- lexeme $ lowVar
    eq <- lexeme $ char ':' 
    ex <- lexeme $ type_
    return (c,ex)

record :: Parser Type -- [(String, Type)] 
record = do 
    fc <- lexeme $ char '{' 
    mc <- lexeme $ try $ sepBy tdataTypecell (lexeme $ char ',') 
    ec <- lexeme $ char '}'
    return $ TRecord mc


tLambda :: Parser Type 
tLambda = do 

    fc <- lexeme $ string "->"
    ec <- lexeme $ type_
    return ec

    -- mc <- lexeme $ try $ sepBy tType (lexeme $ string "->") 

    -- return $ TLambda (head mc) (tail mc)


tType :: Parser Type 
tType = do 
    name <- lexeme $ type_1
    --args <- lexeme $ many type_
    rest <- lexeme $ optionMaybe tLambda
    case rest of 
        Nothing -> return $ name
        Just rr -> return $ TLambda name rr

tTTypeQual  :: Parser Type    
tTTypeQual = do 
    name <- lexeme $ uppVar 
    args <- lexeme $ many type_
    return $ TTypeQual name args

type_1:: Parser Type 
type_1= try variable <|> tuple <|> record  <|> tLambda<|> tTTypeQual



type_ :: Parser Type 
type_ = try variable <|> tuple <|> record <|> tType <|> tLambda<|> tTTypeQual

--types :: Parser [Type]
--types = spaces *> many (lexeme $ type_)

-- TType "Msg" [TTuple (TType "TT" []) (TType "LT" []) [],TTuple (TType "TT" []) (TType "LT" []) []]

