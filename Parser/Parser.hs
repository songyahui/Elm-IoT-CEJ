module Parser.Parser where
import Parser.AST 
import Parser.Declaration
import Text.ParserCombinators.Parsec 
import Control.Applicative ((<*), (*>), (<$>), (<*>))


build_AST :: SourceName -> String -> Either ParseError [Decl]
build_AST = runParser declarations ()
----------------------------------
---- filt all the comments -------
---  clearComments 
---  delete_till_return
----------------------------------
clearComments:: String -> String -> String --(char r[],int pProject)
clearComments [] oupStr =  oupStr
clearComments (x:xs) oupStr
    | x == '-' && ((head xs) == '-') = clearComments (clearComments_helper (tail xs)) oupStr
    | otherwise = clearComments xs (oupStr ++ [x])
   
clearComments_helper:: String -> String
clearComments_helper [] = []
clearComments_helper (x:xs) 
    | x == '\n' = (x:xs) 
    | otherwise = clearComments_helper xs

-----------------------------------------------------
parse :: String -> String -> Either ParseError [Decl]
parse fileName inpStr = 
    let inpStr_no_comments = clearComments inpStr "" 
    in build_AST fileName inpStr_no_comments 