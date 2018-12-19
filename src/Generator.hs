{-# LANGUAGE DeriveGeneric #-}
module Generator where

import Data.List
import Data.Proxy
import GHC.Generics
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

data Program = Program [Clause] deriving (Generic, Show)

data Clause = Clause VarName Body deriving Generic

data Atom = Atom String deriving (Generic, Show)

data Exp = 
      IntExp Int 
    | VarExp VarName
    | ExpBOp Exp BinOp Exp
    deriving (Generic)

data FirstOrderCall = FirstOrderCall VarName deriving Generic
    
data Body = 
        TrueBody
    |   Is Elhs Exp 
    |   BodyBinOp Body BodyBinOp Body
    |   BodyUOp Body
    |   BodyFirstOrderCall FirstOrderCall
    deriving Generic

data BodyBinOp = And | Or | Implies deriving Generic

data BodyUnOp = Not deriving Generic

data BinOp = Plus | Minus | Div deriving Generic

data UnOp = Msb | Abs | Truncate deriving Generic

data Elhs = IntElhs Int | VarElhs VarName deriving Generic
        
data Term = IntTerm Int | VarTerm VarName deriving Generic

newtype VarName = VarName { unwrapVarName :: String } deriving Show

genProgram :: Gen Program
genProgram = Program <$> listOf (genericArbitrary :: Gen Clause)

instance Show Clause where
    show (Clause name body) = "clausedef(" ++ show name ++ ", [], [int, int]).\n\
    \t"++ show name ++"(X, X) :- "++ show body ++"."

instance Show FirstOrderCall where
    show (FirstOrderCall name) = show name ++"(X)."

instance Show Exp where
    show (IntExp i) = show i
    show (VarExp str) = show $ unwrapVarName str
    show (ExpBOp e1 bop e2) = show e1 ++ show bop ++ show e2
    
instance Show Body where
    show TrueBody = "true"
    show (Is elhs exp) = show elhs ++ " is " ++ show exp
    show (BodyBinOp b1 bop b2) = "(" ++ show b1 ++ " " ++ show bop ++ " " ++ show b2 ++ ")"

instance Show BodyBinOp where
    show And = ","
    show Or = ";"
    show Implies = "->"
    
instance Show BinOp where
    show Plus = "+"
    show Minus = "-"
    show Div = "/"

instance Show BodyUnOp where
    show Not = "\\+"

instance Show Elhs where
    show (IntElhs int) = show int
    show (VarElhs str) = show $ unwrapVarName str

instance Show Term where
    show (IntTerm int) = show int
    show (VarTerm str) = show $ unwrapVarName str

instance Arbitrary Program where
  arbitrary = genericArbitrary

instance Arbitrary Clause where
  arbitrary = genericArbitrary

instance Arbitrary Exp where
  arbitrary = genericArbitrary

instance Arbitrary Body where
  arbitrary = genericArbitrary

instance Arbitrary FirstOrderCall where
  arbitrary = genericArbitrary

instance Arbitrary BodyBinOp where
  arbitrary = genericArbitrary

instance Arbitrary BinOp where
  arbitrary = genericArbitrary

instance Arbitrary Elhs where
  arbitrary = genericArbitrary

instance Arbitrary Term where
  arbitrary = genericArbitrary

instance Arbitrary VarName where
  arbitrary = VarName <$> listOf (elements ['a'..'z'] :: Gen Char)

instance ToADTArbitrary Program

instance ToADTArbitrary Clause

instance ToADTArbitrary Exp

instance ToADTArbitrary Body

instance ToADTArbitrary BodyBinOp

instance ToADTArbitrary BinOp

instance ToADTArbitrary Elhs

instance ToADTArbitrary Term

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

extractAdt :: ADTArbitrarySingleton a -> a
extractAdt (ADTArbitrarySingleton _ _ (ConstructorArbitraryPair _ adt)) = adt

generateBody :: [Clause] -> Gen Body
generateBody clauses =
    oneof [
        return TrueBody,
        liftM2 Is arbitrary arbitrary,
        liftM3 BodyBinOp arbitrary arbitrary arbitrary,
        BodyUOp <$> arbitrary,
        BodyFirstOrderCall <$> FirstOrderCall <$> clauseName clauses
    ]
    where
        clauseName clauses = getVarName <$> (oneof . map (return :: Clause -> Gen Clause)) clauses
        getVarName (Clause name _) = name


generateProgram :: Gen Program
generateProgram = do
    clauses <- listOf1 (genericArbitrary :: Gen Clause)
    testClause <- generateTestClause clauses
    return $ Program clauses

generateTestClause :: [Clause] -> Gen Clause
generateTestClause clauses = do
    body <- generateBody clauses
    clause <- (toADTArbitrarySingleton (Proxy :: Proxy Clause))
    return $ extractAdt clause
        
generatePrologFile :: IO String
generatePrologFile = do 
    program <- generate generateProgram
    return $ show program
