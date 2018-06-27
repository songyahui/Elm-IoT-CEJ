import Parser.Parser as P
import Validator as V
import Generator as G
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
               case V.validator astp of 
                    Left ev -> do print ev 
                    Right astv -> 
                        do putStrLn "Validate succecfully!" 
                           hPutStr outFile (G.generator "" astp )
                
    hClose inFile
    hClose outFile

