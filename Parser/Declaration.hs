module Parser.Declaration where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Parser.Pattern 
import Parser.Expression
import Parser.Type as T


unionDhelper :: Parser (String, [Type])
unionDhelper = do 
    fc <- lexeme $ uppVar
    ec <- lexeme $ many $ type_
    return $ (fc,ec)

unionD:: Parser Decl
unionD = do 
    (name, args) <- nameArgsEquals
    ec <- sepBy unionDhelper (lexeme $ char '|') 
    return $ Union name args ec 

aliasD:: Parser Decl 
aliasD = do 
    re <- try $ lexeme $ string "alias" 
    (name, args) <- nameArgsEquals
    ec <- type_
    return $ Alias name args ec

nameArgsEquals :: Parser (String, [String])
nameArgsEquals = do 
    name <- lexeme $ uppVar
    nameArgsEqualsHelp name []


nameArgsEqualsHelp :: String -> [String] -> Parser (String, [String])
nameArgsEqualsHelp name args = (nameArgsEqualsHelp0 name args )<|> (nameArgsEqualsHelp1 name args)
    
nameArgsEqualsHelp0 name args= 
    do  arg <- lexeme $ lowVar
        nameArgsEqualsHelp name (arg:args)

nameArgsEqualsHelp1 name args = 
    do  eq <- lexeme $ char '='
        return $ ( name, reverse args )


unionD_or_aliasD  :: Parser Decl
unionD_or_aliasD = do 
    re <- try $ lexeme $ string "type" 
    fc <- lexeme_ret $ unionD <|> aliasD
    return fc 
    

definition :: String -> Parser Decl
definition fc = do 
    mc <- lexeme $ many $ pattern
    eq <- lexeme $ char '='
    ec <- lexeme_ret $ expr
    return $ Definition fc mc ec


annotation :: String -> Parser Decl
annotation fc = do 
    mc <- lexeme $ char ':'
    ec <- lexeme_ret $ T.type_
    return $ Annotation fc ec

_def  :: Parser Decl--String [Pattern] Expr
_def = do 
    fc <- lexeme $ lowVar 
    mc <- (definition fc)<|> (annotation fc)
    return mc



declaration :: Parser Decl
declaration = try unionD_or_aliasD <|> _def


declarations :: Parser [Decl]
declarations = spaces *> many (lexeme $ declaration)