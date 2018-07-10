import Parser.Parser as P
import Generator.Transformer 
import Generator.Generator
import Text.ParserCombinators.Parsec 
import System.IO 
import System.Environment 

main :: IO ()
main = do
    args <- getArgs
    inFile <- openFile (head args) ReadMode 
    --outFile <- openFile (args !! 1) WriteMode 
    inpStr <- hGetContents inFile

    case P.parse (head args) inpStr of 
        Left ep -> do print ep 
        Right astp -> 
            do print astp 
               putStrLn "-----------Parse succecfully!-----------" 
               print $ transformer astp []
               putStrLn "-----------Transform succecfully!-----------" 
               print $ generator $ transformer astp []
               writeFile (args !! 1) ( generator $ transformer astp [])
               hClose inFile
               --hClose outFile

