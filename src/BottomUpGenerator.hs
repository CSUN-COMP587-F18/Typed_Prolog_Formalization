{-# LANGUAGE DeriveGeneric #-}
module BottomUpGenerator where

import Data.List
import Data.Proxy
import GHC.Generics
import Control.Monad
import Test.QuickCheck
import Control.Monad.State.Lazy
import Test.QuickCheck.Arbitrary.ADT


data GState = GState Gamma Udts
data Gamma = Gamma [(Identifier, Type)]
data Udts = Udts [(Identifier, [CDef])]

newtype Identifier = Identifier { name :: String } 

data Clause = Clause {
    getClauseName :: String,
    getClauseVars :: [Term],
    getClauseBody :: Body
} deriving Generic

newtype Type = Type { identifier :: Identifier} deriving Generic

data CDef = CDef Identifier [Type]

data Adt = Adt Identifier [CDef]

data BinOp = Plus | Minus | Div deriving Generic

data BodyBinOp = And | Or | Implies deriving Generic

data Exp = 
      IntExp Int 
    | VarExp Identifier
    | ExpBOp Exp BinOp Exp
    deriving (Generic)

data Body = 
        TrueBody
    |   Is Elhs Exp 
    |   BodyBinOp Body BodyBinOp Body
    |   BodyUOp Body
    |   FirstOrderCall Clause [Term]
    deriving Generic

data UnOp = Msb | Abs | Truncate deriving Generic

data Elhs = IntElhs Int | VarElhs Identifier deriving Generic
        
data Term = IntTerm Int | VarTerm Identifier deriving Generic

instance Show Identifier where
    show (Identifier name) = name

instance Show Type where
    show (Type (Identifier name)) = name

instance Show Term where
    show (IntTerm i) = show i
    show (VarTerm i) = show i

instance Show Adt where
    show (Adt name cdefs) = 
        "datadef("++ show name ++", [], ["++  (concat $ map show cdefs)  ++"])."

instance Show CDef where
    show (CDef name paramTypes) =
        show name ++ "(" ++ (intercalate ",") (map show paramTypes) ++ ")"

instance Arbitrary Clause where
  arbitrary = genericArbitrary

instance Arbitrary Body where
  arbitrary = genericArbitrary

instance Arbitrary BinOp where
  arbitrary = genericArbitrary

instance Arbitrary BodyBinOp where
  arbitrary = genericArbitrary

instance Arbitrary Exp where
  arbitrary = genericArbitrary

instance Arbitrary Term where
  arbitrary = genericArbitrary

instance Arbitrary Elhs where
  arbitrary = genericArbitrary

instance Arbitrary Type where
  arbitrary = genericArbitrary

instance Arbitrary Identifier where
  arbitrary = Identifier <$> listOf (elements ['a'..'z'] :: Gen Char)

instance ToADTArbitrary Clause

instance ToADTArbitrary Term

instance ToADTArbitrary Type

extractAdt :: ADTArbitrarySingleton a -> a
extractAdt (ADTArbitrarySingleton _ _ (ConstructorArbitraryPair _ adt)) = adt

generateProgram :: State GState (Gen Term)
generateProgram =
    return (genericArbitrary :: Gen Term)

-- generateOutput :: IO (State GState (Gen Term))
generateOutput = do
    term <- fst (runState generateProgram emptyState)
    return $ show term
    where 
        emptyState = GState (Gamma []) (Udts [])

