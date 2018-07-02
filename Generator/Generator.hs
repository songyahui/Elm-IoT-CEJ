module Generator.Generator where
import Parser.AST 
import Generator.JSAST
import Data.Maybe
import Data.List.Split


-----------------------------------------------------
generator :: [Decl] -> String
generator trees =  ""
    --case main_function trees of 
    --    Nothing ->  "Wrong defination of main function"
    --    Just mvu -> analyse_Model mvu trees 