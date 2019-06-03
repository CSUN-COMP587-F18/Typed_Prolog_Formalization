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

newtype Identifier = Identifier { name :: String } deriving Generic

data Clause = Clause {
    getClauseName :: String,
    getClauseVars :: [Term],
    getClauseBody :: Body
} deriving Generic

newtype Type = Type { identifier :: Identifier} deriving Generic

data CDef = CDef Identifier [Type] deriving Generic

data Adt = Adt Identifier [CDef] deriving Generic

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

-- instance Show Clause where
--     show (Clause name vars body) = "clausedef(" ++ show name ++ ", [], ["++ show  (map getTermType vars) ++"]).\n\
--     \t"++ show name ++"(X, X) :- "++ show body ++"."

instance Show Term where
    show (IntTerm i) = show i
    show (VarTerm i) = show i

instance Show Adt where
    show (Adt name cdefs) = 
        "datadef("++ show name ++", [], ["++  (concatMap show cdefs)  ++"])."

instance Show CDef where
    show (CDef name paramTypes) =
        show name ++ "(" ++ (intercalate ",") (map show paramTypes) ++ ")"

instance Arbitrary Clause where
    arbitrary = genericArbitrary

instance Arbitrary Body where
    arbitrary = genericArbitrary

instance Arbitrary BinOp where
    arbitrary = genericArbitrary

instance Arbitrary Adt where
    arbitrary = genericArbitrary

instance Arbitrary CDef where
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

generateAdts :: State GState (Gen [Adt])
generateAdts = do 
    GState _ (Udts alist) <- get
    return $ listOf1 (generateAdtFromExisting $ map fst alist)
    

generateCdefFromExisting :: [Identifier] -> Gen CDef
generateCdefFromExisting identifiers = CDef <$> (arbitrary :: Gen Identifier) <*> (sublistOf types)
    where
        types = map Type identifiers

generateAdtFromExisting :: [Identifier] -> Gen Adt
generateAdtFromExisting types = Adt <$> (arbitrary :: Gen Identifier) <*> listOf (generateCdefFromExisting types)

generateClauses :: State GState (Gen [Clause])
generateClauses =
    return $ listOf1 (genericArbitrary :: Gen Clause)

generateOutput :: IO String
generateOutput = do
    adt <- generate $ evalState generateAdts emptyState
    return $ show adt
    where 
        emptyState = GState (Gamma []) (Udts [])

