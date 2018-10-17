{-# LANGUAGE DeriveGeneric #-}
module Generator (generatePrologFile) where

import Data.Proxy
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT


data Body = Is Term Term deriving (Generic)
instance Show Body where
    show (Is t1 t2) = show t1 ++ " is "++ show t2

instance Arbitrary Body where
  arbitrary = genericArbitrary

instance ToADTArbitrary Body

data Term = IntTerm Int | VarTerm String deriving (Generic)
instance Show Term where
    show (IntTerm int) = show int
    show (VarTerm str) = str

instance Arbitrary Term where
  arbitrary = genericArbitrary

instance ToADTArbitrary Term

extractAdt :: ADTArbitrarySingleton a -> a
extractAdt (ADTArbitrarySingleton _ _ (ConstructorArbitraryPair _ adt)) = adt

generateTerm :: IO Term
generateTerm = do
    term <- generate (toADTArbitrarySingleton (Proxy :: Proxy Term))
    return $ extractAdt term

generateBody :: IO Body
generateBody = do
    body <- generate (toADTArbitrarySingleton (Proxy :: Proxy Body))
    return $ extractAdt body
        
generatePrologFile :: IO String
generatePrologFile = do 
    body <- generateTerm
    return $ "clausedef(test, [], []). \n\
      \test :- \n\t"++ show body ++"."


