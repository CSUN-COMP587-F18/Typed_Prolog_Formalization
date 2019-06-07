module GraphGenerator where

import Test.QuickCheck
import Data.Graph.Inductive.Arbitrary

genGraph = do
    graph <- generate arbitraryGraph
    return $ show graph