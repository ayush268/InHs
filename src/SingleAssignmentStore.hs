-- Module for managing the Single Assignment Store
-- for the Interpreter

module SingleAssignmentStore
  (addVariable) where

import qualified Data.Map as Map
import qualified Data.UUID as UUID
import qualified System.Random as Rand

import qualified Types
import qualified Helpers

addVariable :: Types.SingleAssignmentStore -> Types.MemoryList -> (Types.SingleAssignmentStore, Types.MemoryList)
addVariable sas memory = (((Map.insert var eqClass eqMap), (valueMap)), updatedMemoryList)
  where eqMap             = fst sas
        valueMap          = snd sas
        var               = head memory
        eqClass           = head $ tail memory
        updatedMemoryList = drop 2 memory
