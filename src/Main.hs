module Main where

import Generator (generatePrologFile)
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    contents <- generatePrologFile
    case length args of
        0 -> putStrLn "Needs an argument for file name."
        otherwise -> writeFile (head args) $ contents
    

