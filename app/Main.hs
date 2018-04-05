module Main where

import Text.Printf
import System.Environment (getArgs, getProgName)
import AdtProcessor
import PrettyPrint

main :: IO ()
main = do files <- getArgs
          processFiles files

processFiles :: [String] -> IO ()
processFiles (scanDir : outFile : _) =
 do content <- printAdtFormats <$> (findAndProcessAdts scanDir)
    _ <- writeFile outFile content
    putStrLn ("output written to: " ++ outFile)
processFiles _ =
 do name <- getProgName
    putStrLn $ printf "usage: %s [dir to scan] [outputfile]" name
