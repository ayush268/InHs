-- Module listing out types for the interpreter

module Types
  (Statement(..),
   ValuesRead(..),
   Value(..),
   Identifier,
   Literal,
   Memory,
   EnvironmentMap,
   MemoryToEqClassMap,
   EqClassToValueMap,
   SingleAssignmentStore,
   MemoryList) where

import qualified Data.Map as Map
import qualified Data.UUID as UUID

-- Type Synonyms
type Identifier = String
type Literal = Int
type Memory = Int

type EnvironmentMap = Map.Map Identifier Memory

type MemoryToEqClassMap = Map.Map Memory Memory
type EqClassToValueMap  = Map.Map Memory Value

type SingleAssignmentStore = (MemoryToEqClassMap, EqClassToValueMap)

type MemoryList = [Memory]

type ReadFeatureMap = Map.Map Literal Identifier
type FeatureMap = Map.Map Literal Memory

-- New Types
data Statement = Skip
                 | Multiple {stmts :: [Statement]}
                 | Var {dest :: Identifier,
                        stmt :: Statement}
                 | BindIdent {dest :: Identifier,
                              src  :: Identifier}
                 | BindValue {dest  :: Identifier,
                              value :: ValuesRead}
                 | Conditional {src     :: Identifier,
                                fststmt :: Statement,
                                sndstmt :: Statement}
                 | Match {src     :: Identifier,
                          pattern :: ValuesRead,
                          fststmt :: Statement,
                          sndstmt :: Statement}
                 | Apply {func       :: Identifier,
                          parameters :: [Identifier]} deriving (Eq, Show, Read)

data ValuesRead = Lit {val :: Literal}
                  | Proc {params :: [Identifier],
                          pStmt   :: Statement}
                  | Record {label  :: Literal,
                            values :: ReadFeatureMap} deriving (Eq, Show, Read)

data Value = Liter {litVal :: Literal}
             | Closure {procParameters :: [Identifier],
                        procStmt       :: Statement,
                        procEnv        :: EnvironmentMap}
             | Rec {recLabel  :: Literal,
                    recValues :: FeatureMap} deriving (Show, Read)

-- Making instance of a typeclass

matchRecords :: Value -> Value -> Bool
matchRecords (Rec a b) (Rec c d) = labelMatch && arityMatch && featuresMatch
  where labelMatch = (a == c)
        arityMatch = ((length b) == (length d))
        featuresMatch = ((length $ Map.union b d) == (length $ Map.intersection b d))

matchRecords _ _ = False

instance Eq Value where
  (Liter x) == (Liter y) = x == y
  (Rec a b) == (Rec c d) = (matchRecords (Rec a b) (Rec c d))
  _ == _ = False
