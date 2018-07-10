module Generator.Generator where
import Generator.Transformer
import Parser.AST 
import Generator.JSAST
import Data.Maybe
import Data.List.Split

generate  :: JSAST -> String
generate js_Ast = "done"
-----------------------------------------------------

generator :: [Decl] -> [String] -> JSAST
generator trees fun_names= transformer trees fun_names
    --let js_Ast = transformer trees fun_names
    --in generate js_Ast
    --case main_function trees of 
    --    Nothing ->  "Wrong defination of main function"
    --    Just mvu -> analyse_Model mvu trees 