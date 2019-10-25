-- Module for handling functions based on Multi Stack and
-- Concurrency features,
-- Part of Declarative Concurrent Model

module Concurrency
  (isBound) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Types
import qualified Helpers
import qualified SingleAssignmentStore as SAS

isBound :: Types.Identifier -> Types.EnvironmentMap -> Types.SingleAssignmentStore -> Bool
isBound src env (eqMap, valueMap)
  | (Maybe.isJust $ Map.lookup src env) == True = if (Maybe.isJust eqClass)
                                                    then (Maybe.isJust $ Map.lookup (Maybe.fromJust eqClass) valueMap)
                                                    else False
  | otherwise = False
  where var = Map.lookup src env
        eqClass = Map.lookup (Maybe.fromJust var) eqMap
