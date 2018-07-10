import Parser.Parser as P
import Validator.Validator as V
import Generator.Generator as G
import Text.ParserCombinators.Parsec 
import System.IO 
import System.Environment 

main :: IO ()
main = do
    args <- getArgs
    inFile <- openFile (head args) ReadMode 
    outFile <- openFile (args !! 1) WriteMode 
    inpStr <- hGetContents inFile

    case P.parse (head args) inpStr of 
        Left ep -> do print ep 
        Right astp -> 
            do putStrLn "Parse succecfully!" 
               print astp 
            --    case V.validator astp of 
            --         Left ev -> do print ev 
            --         Right fun_names -> 
            --             do  print fun_names
            --                 putStrLn "Validate succecfully!\n" 
            --                 hPutStr outFile $ show (G.generator astp fun_names)
            --                 putStrLn $ show $ G.generator astp fun_names
    hClose inFile
    hClose outFile

