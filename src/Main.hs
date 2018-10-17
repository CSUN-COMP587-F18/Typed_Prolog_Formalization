module Main where

import Generator (generatePrologFile)
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    writeFile (head args) $ generatePrologFile "true"

