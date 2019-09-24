-- Module listing out types for the interpreter

module Types
  (Statement(..),
   Value(..),
   Identifier,
   Literal,
   EnvironmentMap,
   MemoryToEqClassMap,
   EqClassToValueMap) where

import qualified Data.Map as Map
import qualified Data.UUID as UUID

-- Type Synonyms
type Identifier = String
type Literal = Int

type EnvironmentMap = Map.Map String UUID.UUID

type MemoryToEqClassMap = Map.Map UUID.UUID UUID.UUID
type EqClassToValueMap  = Map.Map UUID.UUID Value

-- New Types
data Statement = Skip
                 | Multiple {stmts :: [Statement]}
                 | Var {ident :: Identifier,
                        stmt  :: Statement}
                 | Bind {ident   :: Identifier,
                         literal :: Literal}
                 | Record {label :: Literal,
                           pairs :: [(Literal, Identifier)]}
                 | Proc {parameters :: [Identifier],
                         stmt       :: Statement}
                 | Conditional {ident   :: Identifier,
                                fststmt :: Statement,
                                sndstmt :: Statement}
                 | Match {ident   :: Identifier,
                          fststmt :: Statement,
                          sndstmt :: Statement}
                 | Apply {func       :: Identifier,
                          parameters :: [Identifier]} deriving (Show)

data Value = Lit Literal
             | Closure {procParameters :: [Identifier],
                        procStmt       :: Statement,
                        procEnv        :: EnvironmentMap}
             | Rec {recLabel  :: Literal,
                    recValues :: [(Literal, Identifier)]}

-- instance Read Statement where
