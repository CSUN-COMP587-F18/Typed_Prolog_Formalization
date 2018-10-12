module Generator (generate) where

generateTerm :: Int -> Int -> String
generateTerm x state =
    "T" ++ show (state + 1)

generate body
    = "clausedef(test, [], []). \n\
      \test :- \n\t"++ body ++"."



