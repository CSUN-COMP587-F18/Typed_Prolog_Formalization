{-# LANGUAGE DeriveGeneric #-}
module Generator (generatePrologFile) where

import Data.Proxy
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

data Atom = Atom String deriving (Generic, Show)

data Exp = 
      IntExp Int 
    | VarExp VarName
    | ExpBOp Exp BinOp Exp
    deriving (Generic)

data FirstOrderCall = FirstOrderCall VarName deriving Generic
    
data Body = 
        Is Elhs Exp 
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

genVarChar :: Gen Char
genVarChar = elements ['a'..'z']

genVarName :: Gen String
genVarName = listOf genVarChar

instance Show FirstOrderCall where
    show _ = "uClause(X)."

instance Show Exp where
    show (IntExp i) = show i
    show (VarExp str) = show $ unwrapVarName str
    show (ExpBOp e1 bop e2) = show e1 ++ show bop ++ show e2
    
instance Show Body where
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
    show Not b = show b

instance Show Elhs where
    show (IntElhs int) = show int
    show (VarElhs str) = show $ unwrapVarName str

instance Show Term where
    show (IntTerm int) = show int
    show (VarTerm str) = show $ unwrapVarName str


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
  arbitrary = VarName <$> genVarName

instance ToADTArbitrary Exp

instance ToADTArbitrary Body

instance ToADTArbitrary BodyBinOp

instance ToADTArbitrary BinOp

instance ToADTArbitrary Elhs

instance ToADTArbitrary Term

extractAdt :: ADTArbitrarySingleton a -> a
extractAdt (ADTArbitrarySingleton _ _ (ConstructorArbitraryPair _ adt)) = adt

generateBody ::IO Body
generateBody = do
    term <- generate (toADTArbitrarySingleton (Proxy :: Proxy Body))
    return $ extractAdt term
        
generatePrologFile :: IO String
generatePrologFile = do 
    body <- generateBody
    return $ "clausedef(uClause, [], [int]). \n\
            \uClause(X) :- true.\n\
        \clausedef(test, [], []). \n\
            \test :- \n\t"++ show body  ++"."
