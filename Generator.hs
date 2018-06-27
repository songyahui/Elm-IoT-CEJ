module Generator where
import Parser.AST 
import Data.Maybe
import Data.List.Split



ge_decl_Union :: String -> [String] -> [(String, [Type])] -> String
ge_decl_Union name params li_cons = "union\n"

ge_decl_defination :: String -> [Pattern] -> Expr -> String
ge_decl_defination name params expr = "decl\n"


ge_decl:: String -> Decl -> String
ge_decl temp decl = -- "decl\n" 
    case decl of 
        Union name params li_cons -> ge_decl_Union name params li_cons 
        Definition name params expr -> ge_decl_defination name params expr 


generator :: String -> [Decl] -> String
generator temp [] = temp
generator temp (x:xs) = generator (temp ++ ge_decl "" x) xs -- Right "done"


