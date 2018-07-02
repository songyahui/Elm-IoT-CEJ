module Validator.Validator where
import Parser.AST 

find_func_by_name :: String -> [Decl] -> Maybe Decl
find_func_by_name name [] = Nothing
find_func_by_name name (x:xs) = 
    case x of 
    Definition a b c -> if a == name then Just x
                        else find_func_by_name name xs
    Union aa bb cc ->   if aa == name then Just x
                        else find_func_by_name name xs

-----------------------------------------------------
get_func_names :: [(String,Expr)] -> Maybe [String]
get_func_names fun_lists = 
    let exact fun_name f_l = 
            case f_l of
            [] -> Nothing
            (x:xs) -> if (fst x) == fun_name 
                      then case snd x of
                            Var name -> Just name
                      else exact fun_name xs
             
        model = (exact "model" fun_lists)
        view = (exact "view" fun_lists)
        update = (exact "update" fun_lists)
    in  case model of 
        Nothing -> Nothing
        Just m -> case view of 
                  Nothing -> Nothing
                  Just v -> case update of 
                            Nothing -> Nothing
                            Just u -> Just ([m] ++ [v] ++ [u])
 
main_function :: [Decl] -> Maybe [String]
main_function ast = 
    case find_func_by_name "main" ast of
        Nothing -> Nothing
        Just m -> case m of 
            Definition a b c -> case c of 
                Record l -> get_func_names l 
-----------------------------------------------------
validator :: [Decl] -> Either String [String]
validator ast = -- Right ast --Left ("Undefined variable ")
    case main_function ast of 
        Nothing ->  Left "Wrong defination inside of main function"
        Just mvu -> Right mvu -- "analyse_Model mvu ast "-- analyse_Model mvu ast 

