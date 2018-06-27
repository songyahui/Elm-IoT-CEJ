module Parser.Expression where
import Parser.AST
import Parser.Pattern 
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))

----------------------------------------
eStr :: Parser Expr-- Char
eStr = do
    fc    <- char '\"'
    mc  <- many $ oneOf anyChar
    ec   <- lexeme $ char '\"'
    return $ Str mc
  where anyChar = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++"_" ++ "-" ++ "." ++ " " 

----------------------------------------
decimalNumE :: Parser Expr
decimalNumE = do
    fc  <- lexeme $ many1 $ oneOf numChar
    mc  <- optionMaybe $ char '.'
    case mc of 
        Nothing -> return $ Int (read fc)
        Just dot -> do 
                ec <- lexeme $ many1 $ oneOf numChar
                return $ Float (read (fc++"."++ec))
  where numChar = ['0'..'9']

hexNumE :: Parser Expr
hexNumE = do
    fc  <- lexeme $ string "0X"
    rest  <- lexeme $ many1 $ oneOf numChar
    return $ Int (read $ fc ++ rest)
  where numChar = ['0'..'9']

eInt :: Parser Expr-- Int
eInt = try hexNumE <|> decimalNumE

----------------------------------------
eVar :: Parser Expr-- String
eVar = do
    fc  <- oneOf fch
    r   <- lexeme $ many $ oneOf rest
    case ([fc] ++ r) of 
        "True" -> return $ Boolean True
        "False" -> return $ Boolean False
        otherwise -> return $ Var ([fc] ++ r)
    where fch =  ['A'..'Z'] ++ ['a'..'z'] ++ "_"
          rest = fch ++ ['0'..'9']

eVar0 :: Parser String-- String
eVar0 = do
    fc  <- oneOf fch
    r   <- lexeme $ many $ oneOf rest
    return $  ([fc] ++ r)
    where fch =  ['A'..'Z'] ++ ['a'..'z'] ++ "_"
          rest = fch ++ ['0'..'9']


eNegate :: Parser Expr -- Expr
eNegate = do
    fc    <- lexeme $ char '-'
    mc    <- lexeme $ expr
    return $ Negate mc     

eBinops :: Parser Expr-- String Expr Expr
eBinops = do
    fe    <- lexeme $ expr0
    mc    <- try (lexeme $ string "+") <|> (lexeme $ string "-") <|> (lexeme $ string "*") <|> (lexeme $ string "/") <|> (lexeme $ string "=") <|> (lexeme $ string ">") <|> (lexeme $ string "<")  
    ec    <- lexeme $ expr
    return $ Binops mc fe ec  
----------------------------------------
elseifE :: Parser (Expr,Expr)
elseifE = do 
    elseif_word <- lexeme $ string "else"
    elseif_word <- lexeme $ string "if"
    elseif_ <- lexeme $ expr
    then_word <- lexeme $ string "then"
    then_ <- lexeme $ expr
    return $ (elseif_,then_)

eIf :: Parser Expr-- [(Expr, Expr)] Expr
eIf = do
    if_word <- try $ lexeme $ string "if"
    if_ <- lexeme $ expr
    then_word <- lexeme $ string "then"
    then_ <- lexeme $ expr0
    
    manyelseif <- many $ try elseifE
    
    else_word <- lexeme $ string "else"
    else_ <- lexeme $ expr0
    
    return $ If ([(if_,then_)]++manyelseif) else_
----------------------------------------
dataTypecell :: Parser (String,Expr)
dataTypecell = do 
    c <- lexeme $ eVar0
    eq <- lexeme $ char '=' 
    ex <- lexeme $ expr0
    return (c,ex)

eRecord :: Parser Expr
eRecord = do 
    mc <- try $ lexeme $ try $ sepBy dataTypecell (lexeme $ char ',') 
    ec <- try $ lexeme $ char '}'
    return $ Record mc

eUpdate :: Parser Expr
eUpdate = do 
    fc <- lexeme $ eVar0
    eq <- lexeme $ char '|' 
    mc <- try $ lexeme $ try $ sepBy dataTypecell (lexeme $ char ',') 
    ec <- try $ lexeme $ char '}'
    return $ Update fc mc


eRecord_Update :: Parser Expr -- [(String, Expr)]
eRecord_Update = do 
    fc <- try $ lexeme $ char '{' 
    ec <- lexeme $ eRecord <|> eUpdate
    return ec

eList :: Parser Expr-- [Expr]
eList = do 
    lb <- lexeme $ char '[' 
    content <- try $ sepBy expr (lexeme $ char ',') 
    rb <- lexeme $ char ']' 
    return $ List content

eLambda :: Parser Expr-- [Pattern] Expr
eLambda = do 
    fc <- lexeme $ char '\\'
    mc <- lexeme $ many $ pattern
    eq <- lexeme $ string "->"
    ec <- lexeme $ expr
    return $ Lambda mc ec

eLet :: Parser Expr-- [Def] Expr
eLet = do
    let_ <- try $ lexeme $ string "let"
    fun_<- lexeme $ many1 $ try defination
    in_ <- lexeme $ string "in"
    expr_<- lexeme $ expr
    -- case_body <- lexeme $ many1 $ try case_bodyE
    return $ Let fun_ expr_--case_object case_body

case_bodyE :: Parser (Pattern,Expr)
case_bodyE = do 
    fp <- lexeme $ pattern
    arrowc <- lexeme $ string "->"
    ec <- lexeme $ expr
    return $ (fp , ec)

eCase :: Parser Expr-- Expr [(Pattern, Expr)]
eCase = do
    casec <- try $  lexeme $ string "case"
    case_object<- lexeme $ expr0
    thenc <- lexeme $ string "of"
    case_body <- lexeme $ many1 $ try case_bodyE
    return $ Case case_object case_body

eTupple :: Parser Expr --Expr Expr [Expr]
eTupple = do 
    lb <- lexeme $ char '(' 
    first <- lexeme $ expr0
    cb1 <- lexeme $ char ',' 
    second <- lexeme $ expr0
    cb2 <- optionMaybe $ lexeme $ char ',' 
    case cb2 of 
        Nothing -> do 
            rb1 <- lexeme $ char ')' 
            return $ Tupple first second []
        Just dot -> do 
            rest <- try $ sepBy expr0 (lexeme $ char ',') 
            rb2 <- lexeme $ char ')' 
            return $ Tupple first second rest





eCall :: Parser Expr -- Expr [Expr]
eCall = do 
    fe <- try $ lexeme $ lowVar
    ec <- try $ lexeme $ many expr
    return $ Call fe ec

----------------------------------------
expr0 :: Parser Expr
expr0 = try eStr <|> eInt <|> eVar <|> eNegate <|> eList 

expr :: Parser Expr
expr = try eBinops <|> eTupple <|>eIf <|> eLet <|> eCase<|> eRecord_Update <|> eLambda  <|> eCall <|> expr0

--exprs :: Parser [Expr]
--exprs = spaces *> many (lexeme $ expr)
--------------------

defination :: Parser Def --(String) [Pattern] Expr
defination = do 
    fc <- lexeme $ lowVar
    mc <- lexeme $ many $ pattern
    eq <- lexeme $ char '='
    ec <- lexeme $ expr
    return $ Define fc mc ec