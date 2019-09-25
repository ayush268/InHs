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
                          pattern :: Value,
                          fststmt :: Statement,
                          sndstmt :: Statement}
                 | Apply {func       :: Identifier,
                          parameters :: [Identifier]} deriving (Eq, Show)

data ValuesRead = Lit {val :: Literal}
                  | Proc {params :: [Identifier],
                          pStmt   :: Statement}
                  | Record {label  :: Literal,
                            values :: ReadFeatureMap} deriving (Eq, Show)

data Value = Liter {litVal :: Literal}
             | Closure {procParameters :: [Identifier],
                        procStmt       :: Statement,
                        procEnv        :: EnvironmentMap}
             | Rec {recLabel  :: Literal,
                    recValues :: FeatureMap} deriving (Eq, Show)

-- instance Read Statement where
-- instance (Read a) => Read (Statement a) where
