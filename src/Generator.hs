{-# LANGUAGE DeriveGeneric #-}
module Generator where

import Data.List
import Data.Proxy
import GHC.Generics
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.ADT

data Program = Program [Clause] deriving (Generic, Show)

getClauseTermTypes :: Clause -> [PType]
getClauseTermTypes clause = map getTermType $ getClauseVars clause

data Clause = Clause {
    getClauseName :: String,
    getClauseVars :: [Term],
    getClauseBody :: Body
} deriving Generic

data Udt = 
        UdtA Atom Int Int Int
    |   UdtB Atom Int Int
    |   UdtC Atom Int
    |   UdtD Atom
    deriving (Generic)

instance Show Udt where
    show (UdtA _ _ _ _) = "(atom, int, int int)"
    show (UdtB _ _ _)   = "(atom, int, int)"
    show (UdtC _ _)     = "(atom, int)"
    show (UdtD _)       = "atom"

data UdtDef = UdtDefA 

-- instance Show Udt where
--     show UdtA a i j 

data Atom = Atom String deriving (Generic, Show)

data PType = PInt | PAtom | PUdt Udt deriving (Generic)

data Exp = 
      IntExp Int 
    | VarExp VarName
    | ExpBOp Exp BinOp Exp
    deriving (Generic)

data FirstOrderCall = FirstOrderCall Clause [Term] deriving Generic
    
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
        
data Term = IntTerm Int | VarTerm VarName PType deriving Generic

getTermType :: Term -> PType
getTermType (IntTerm _) = PInt
getTermType (VarTerm _ t) = t 

newtype VarName = VarName { unwrapVarName :: String } deriving (Show)

genProgram :: Gen Program
genProgram = Program <$> listOf (genericArbitrary :: Gen Clause)

instance Show Clause where
    show (Clause name vars body) = "clausedef(" ++ show name ++ ", [], ["++ show  (map getTermType vars) ++"]).\n\
    \t"++ show name ++"(X, X) :- "++ show body ++"."

instance Show FirstOrderCall where
    show (FirstOrderCall clause vars) = getClauseName clause ++"("++ show (head vars) ++", "++ show (head vars) ++")."

instance Show PType where
    show PInt = "int"
    show PAtom = "atom"
    show (PUdt a) = show a

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
    show (VarTerm str _) = show $ unwrapVarName str

instance Arbitrary Atom where
  arbitrary = genericArbitrary

instance Arbitrary Udt where
  arbitrary = genericArbitrary

instance Arbitrary PType where
  arbitrary = genericArbitrary

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

instance ToADTArbitrary Atom

instance ToADTArbitrary Udt

instance ToADTArbitrary PType

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


generateBody :: [Clause] -> [Term] -> Gen Body
generateBody clauses vars =
    oneof [
        return TrueBody,
        liftM2 Is arbitrary arbitrary,
        liftM3 BodyBinOp arbitrary arbitrary arbitrary,
        BodyUOp <$> arbitrary
        -- BodyFirstOrderCall <$> FirstOrderCall <*> clause  <$> [var]
    ]
    where
        clause = (oneof . map (return :: Clause -> Gen Clause)) clauses
        var = (oneof . map (return :: Term -> Gen Term)) vars


generateProgram :: Gen Program
generateProgram = do
    vars <- listOf1 $ oneof [VarTerm <$> arbitrary <*> arbitrary]
    clauses <- listOf1 (genericArbitrary :: Gen Clause)
    testClause <- generateTestClause clauses vars
    return $ Program clauses

generateTestClause :: [Clause] -> [Term] -> Gen Clause
generateTestClause clauses vars = do
    body <- generateBody clauses vars
    clause <- (toADTArbitrarySingleton (Proxy :: Proxy Clause))
    return $ extractAdt clause
        
generatePrologFile :: IO String
generatePrologFile = do 
    program <- generate generateProgram
    return $ show program
