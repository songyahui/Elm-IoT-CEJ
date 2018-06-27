module Parser.Declaration where
import Parser.AST
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Parser.Pattern 
import Parser.Expression
import Parser.Type


unionDhelper :: Parser (String, [Type])
unionDhelper = do 
    fc <- lexeme $ uppVar
    ec <- lexeme $ many $ type_
    return $ (fc,ec)



unionD  :: Parser Decl--String [String] [(String, [Type])]
unionD = do 
    re <- try $ lexeme $ string "type"
    fc <- lexeme $ uppVar
    sc <- lexeme $ many $ lowVar
    eq <- lexeme $ char '='
    ec <- sepBy unionDhelper (lexeme $ char '|') 
    return $ Union fc sc ec 


definitionD  :: Parser Decl--String [Pattern] Expr
definitionD = do 
    fc <- lexeme $ lowVar
    mc <- lexeme $ many $ pattern
    eq <- lexeme $ char '='
    ec <- lexeme $ expr
    return $ Definition fc mc ec

declaration :: Parser Decl
declaration = try  unionD <|> definitionD 


declarations :: Parser [Decl]
declarations = spaces *> many (lexeme $ declaration)