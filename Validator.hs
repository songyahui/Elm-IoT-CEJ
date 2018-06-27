module Validator where
import Parser.AST 

validator :: [Decl] -> Either String [Decl]
validator ast = Right ast--Left ("Undefined variable ")

