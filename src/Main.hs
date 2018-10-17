module Main where

import Generator (generatePrologFile)
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    contents <- generatePrologFile
    writeFile (head args) $ contents

