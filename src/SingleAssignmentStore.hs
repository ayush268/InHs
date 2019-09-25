-- Module for managing the Single Assignment Store
-- for the Interpreter

module SingleAssignmentStore
  (addVariable,
   unifyVariables,
   bindValue) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Types
import qualified Helpers

addVariable :: Types.SingleAssignmentStore -> Types.MemoryList -> (Types.SingleAssignmentStore, Types.MemoryList)
addVariable sas memory = (((Map.insert var eqClass eqMap), (valueMap)), updatedMemoryList)
  where eqMap             = fst sas
        valueMap          = snd sas
        var               = head memory
        eqClass           = head $ tail memory
        updatedMemoryList = drop 2 memory

unifyVariables :: Types.SingleAssignmentStore -> Types.Memory -> Types.Memory -> Types.SingleAssignmentStore
unifyVariables (eqMap, valueMap) x y
  | (Maybe.isNothing (Map.lookup x eqMap)) || (Maybe.isNothing (Map.lookup y eqMap)) = error "Unification Failed: Variable not in SAS"
  | otherwise = unify eqMap valueMap x y

unify :: Types.MemoryToEqClassMap -> Types.EqClassToValueMap -> Types.Memory -> Types.Memory -> Types.SingleAssignmentStore
unify eqMap valueMap x y
  | (Maybe.isNothing (Map.lookup eqX valueMap)) && (Maybe.isNothing (Map.lookup eqY valueMap)) = ((Map.insert y eqX eqMap), valueMap)
  | (Maybe.isJust (Map.lookup eqX valueMap)) && (Maybe.isNothing (Map.lookup eqY valueMap))    = ((Map.insert y eqX eqMap), valueMap)
  | (Maybe.isNothing (Map.lookup eqX valueMap)) && (Maybe.isJust (Map.lookup eqY valueMap))    = ((Map.insert x eqY eqMap), valueMap)
  | otherwise = unifyBounded eqMap valueMap x y
  where eqX = Maybe.fromJust (Map.lookup x eqMap)
        eqY = Maybe.fromJust (Map.lookup y eqMap)

unifyBounded :: Types.MemoryToEqClassMap -> Types.EqClassToValueMap -> Types.Memory -> Types.Memory -> Types.SingleAssignmentStore
unifyBounded eqMap valueMap x y
  | (valX == valY) = ((Map.insert y eqX eqMap), valueMap)
  | otherwise = error "Unification Failed: Variables bound to incompatible types cannot be unified!"
  where eqX  = Maybe.fromJust (Map.lookup x eqMap)
        eqY  = Maybe.fromJust (Map.lookup y eqMap)
        valX = Maybe.fromJust (Map.lookup eqX valueMap)
        valY = Maybe.fromJust (Map.lookup eqY valueMap)

bindValue :: Types.SingleAssignmentStore -> Types.Memory -> Types.ValuesRead -> Types.EnvironmentMap -> Types.SingleAssignmentStore
bindValue (eqMap, valueMap) x value env
  | Maybe.isNothing (Map.lookup eqX valueMap) = (eqMap, (Map.insert eqX newValue valueMap))
  | otherwise = unifyValue (eqMap, valueMap) x newValue
  where eqX = Maybe.fromJust (Map.lookup x eqMap)
        newValue = Helpers.convertValuesReadToValue value env

unifyValue :: Types.SingleAssignmentStore -> Types.Memory -> Types.Value -> Types.SingleAssignmentStore
unifyValue (eqMap, valueMap) x value
  | (value == valX) = (eqMap, valueMap)
  | otherwise = error "Unification Failed: Variable already bound to a different value!"
  where eqX  = Maybe.fromJust (Map.lookup x eqMap)
        valX = Maybe.fromJust (Map.lookup eqX valueMap)
