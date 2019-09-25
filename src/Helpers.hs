-- Module for helper functions

module Helpers
  (isLit,
   isRec,
   matchRecords) where

import qualified Data.Map as Map

import qualified Types

isLit :: Types.Value -> Bool
isLit (Types.Lit x) = True
isLit _ = False

isRec :: Types.Value -> Bool
isRec (Types.Rec x y) = True
isRec _ = False

matchRecords :: Types.Value -> Types.Value -> Bool
matchRecords (Types.Rec a b) (Types.Rec c d) = labelMatch && arityMatch && featuresMatch
  where labelMatch = (a == c)
        arityMatch = ((length b) == (length d))
        featuresMatch = ((length $ Map.union b d) == (length $ Map.intersection b d))

matchRecords _ _ = False
