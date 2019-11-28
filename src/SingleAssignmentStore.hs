-- Module for managing the Single Assignment Store
-- for the Interpreter

module SingleAssignmentStore
  (addVariable,
   unifyVariables,
   bindValue,
   unifyValue) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Types
import qualified Helpers


-- ####################################################################################################
-- Functions to declare new variable (by allocating memory for it)
-- And to bind a variable (in memory) to a valid Value
-- ####################################################################################################

-- addVariable allocates memory for a new variable in the Single Assignment Store and updates Free Memory List
addVariable :: Types.SingleAssignmentStore -> Types.MemoryList -> (Types.SingleAssignmentStore, Types.MemoryList)
addVariable sas memory = (((Map.insert var eqClass eqMap), (valueMap)), updatedMemoryList)
  where eqMap             = fst sas
        valueMap          = snd sas
        var               = head memory
        eqClass           = head $ tail memory
        updatedMemoryList = drop 2 memory

-- bindValue binds a memory address (is SAS) to a valid Value, converted from a read Value
bindValue :: Types.SingleAssignmentStore -> Types.Memory -> Types.ValuesRead -> Types.EnvironmentMap -> Maybe Types.SingleAssignmentStore
bindValue (eqMap, valueMap) x value env
  | Maybe.isNothing newValue = Nothing
  | Maybe.isNothing (Map.lookup eqX valueMap) = Just (eqMap, (Map.insert eqX (Maybe.fromJust newValue) valueMap))
  | otherwise = Just $ unifyValue (eqMap, valueMap) x (Maybe.fromJust newValue)
  where eqX = Maybe.fromJust (Map.lookup x eqMap)
        newValue = Helpers.convertValuesReadToValue value env (eqMap, valueMap)

-- ####################################################################################################


-- ####################################################################################################
-- UNIFICATION Algorithm (This is where the MAGIC happens) (unify variables when bound to value / ident)
-- ####################################################################################################

-- unifyVariables takes SAS and 2 variables (memory addresses), unifies them and returns updated SAS
unifyVariables :: Types.SingleAssignmentStore -> Types.Memory -> Types.Memory -> Types.SingleAssignmentStore
unifyVariables (eqMap, valueMap) x y
  | (Maybe.isNothing (Map.lookup x eqMap)) || (Maybe.isNothing (Map.lookup y eqMap)) = error "Unification Failed: Variable not in SAS"
  | otherwise = unify (eqMap, valueMap) x y

-- unify, the core part of the unification algorithm
-- It considers 3 cases - namely both unbound, one of the variables is bound and both are bound,
unify :: Types.SingleAssignmentStore -> Types.Memory -> Types.Memory -> Types.SingleAssignmentStore
unify (eqMap, valueMap) x y
  | (Maybe.isNothing (Map.lookup eqX valueMap)) && (Maybe.isNothing (Map.lookup eqY valueMap)) = (Helpers.updateOldEquivalenceClass eqY eqX eqMap, valueMap)
  | (Maybe.isJust (Map.lookup eqX valueMap)) && (Maybe.isNothing (Map.lookup eqY valueMap))    = (Helpers.updateOldEquivalenceClass eqY eqX eqMap, valueMap)
  | (Maybe.isNothing (Map.lookup eqX valueMap)) && (Maybe.isJust (Map.lookup eqY valueMap))    = (Helpers.updateOldEquivalenceClass eqX eqY eqMap, valueMap)
  | otherwise = unifyBounded eqMap valueMap x y
  where eqX = Maybe.fromJust (Map.lookup x eqMap)
        eqY = Maybe.fromJust (Map.lookup y eqMap)

-- Special function to take care of the case when both variables are bounded,
-- If variables bound to records, then they have to be dealt with specially
unifyBounded :: Types.MemoryToEqClassMap -> Types.EqClassToValueMap -> Types.Memory -> Types.Memory -> Types.SingleAssignmentStore
unifyBounded eqMap valueMap x y
  | (Helpers.isRec valX) && (Helpers.isRec valY) && (valX == valY) = unifyRecords (eqMap, valueMap) x y
  | (valX == valY) = (Helpers.updateOldEquivalenceClass eqY eqX eqMap, valueMap)
  | otherwise = error "Unification Failed: Variables bound to incompatible types or different values cannot be unified!"
  where eqX  = Maybe.fromJust (Map.lookup x eqMap)
        eqY  = Maybe.fromJust (Map.lookup y eqMap)
        valX = Maybe.fromJust (Map.lookup eqX valueMap)
        valY = Maybe.fromJust (Map.lookup eqY valueMap)

-- When variables are bound to records, unfication will have to be called recursively,
-- for each variable (corresponding to a feature of the record)
unifyRecords :: Types.SingleAssignmentStore -> Types.Memory -> Types.Memory -> Types.SingleAssignmentStore
unifyRecords (eqMap, valueMap) x y = (Helpers.updateOldEquivalenceClass eqY eqX updatedEqMap, updatedValueMap)
  where eqX = Maybe.fromJust (Map.lookup x eqMap)
        eqY  = Maybe.fromJust (Map.lookup y eqMap)
        valX = Maybe.fromJust (Map.lookup eqX valueMap)
        valY = Maybe.fromJust (Map.lookup eqY valueMap)
        (updatedEqMap, updatedValueMap) = foldl (\sas (x, y) -> unify sas x y) (eqMap, valueMap) featureValueMapping
        featureValueMapping = zip (Map.elems $ Types.recValues valX) (Map.elems $ Types.recValues valY)

-- Special case of unification, unifying a value with a variable which is bound,
-- The value of the bound variable must be exactly equal to value supplied in case of literals,
-- Records are a special case.
unifyValue :: Types.SingleAssignmentStore -> Types.Memory -> Types.Value -> Types.SingleAssignmentStore
unifyValue (eqMap, valueMap) x value
  | (Helpers.isRec valX) && (Helpers.isRec value) && (value == valX) = unifyRecordsValues (eqMap, valueMap) x value
  | (value == valX) = (eqMap, valueMap)
  | otherwise = error "Unification Failed: Variable already bound to a different value!"
  where eqX  = Maybe.fromJust (Map.lookup x eqMap)
        valX = Maybe.fromJust (Map.lookup eqX valueMap)

-- Unifying Record value of a bound variable with a Record value supplied as input,
-- will trigger unification for the variables / values corresponding to a feature.
unifyRecordsValues :: Types.SingleAssignmentStore -> Types.Memory -> Types.Value -> Types.SingleAssignmentStore
unifyRecordsValues (eqMap, valueMap) x value = (updatedEqMap, updatedValueMap)
  where eqX = Maybe.fromJust (Map.lookup x eqMap)
        valX = Maybe.fromJust (Map.lookup eqX valueMap)
        (updatedEqMap, updatedValueMap) = foldl (\sas (x, y) -> unify sas x y) (eqMap, valueMap) featureValueMapping
        featureValueMapping = zip (Map.elems $ Types.recValues valX) (Map.elems $ Types.recValues value)

-- ####################################################################################################
