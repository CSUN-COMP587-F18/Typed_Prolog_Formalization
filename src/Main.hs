module Main where

import Generator (generate)
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    writeFile (head args) $ generate "true"

