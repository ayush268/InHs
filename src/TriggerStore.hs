-- Module for managing the Trigger Store
-- for the Interpreter

module TriggerStore
  (activateTriggers,
   addNewTrigger) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import qualified Types

activateTriggers :: Types.SingleAssignmentStore -> Types.TriggerStore -> [Types.Memory] -> (Types.TriggerStore, [(Types.Stack, Types.StackState, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Identifier)])
activateTriggers (eqMap, valueMap) triggerStore listOfSuspendedAddresses = (updatedTriggerStore, newStackTuples)
  where mapFromEqClassesToSuspendedVar = Map.fromList $ map (\x -> (Maybe.fromJust (Map.lookup x eqMap), x)) listOfSuspendedAddresses
        mapFromEqClassesToTriggerStoreVar = Map.fromList $ map (\x -> (Maybe.fromJust (Map.lookup x eqMap), x)) $ Map.keys triggerStore
        triggersToActivate = Map.union (Map.intersection mapFromEqClassesToSuspendedVar mapFromEqClassesToTriggerStoreVar) (Map.filterWithKey (\k _ -> Maybe.isJust $ Map.lookup k valueMap) mapFromEqClassesToTriggerStoreVar)
        triggersLeft = Map.keysSet $ Map.difference mapFromEqClassesToTriggerStoreVar triggersToActivate
        updatedTriggerStore = Map.filterWithKey (\k _ -> Set.member (Maybe.fromJust (Map.lookup k eqMap)) triggersLeft) triggerStore
        newStackTuples = Map.foldlWithKey (\current var closureList -> current ++ (generateNewStackTuples closureList var)) [] $ Map.difference triggerStore updatedTriggerStore

generateNewStackTuples :: [Types.Value] -> Types.Memory -> [(Types.Stack, Types.StackState, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Identifier)]
generateNewStackTuples closureList x = map (\(Types.Closure params stmt env) -> ([(stmt, Map.union (Map.fromList [(head params, x)]) env)], Types.Ready, [], [], [], [], "")) closureList

addNewTrigger :: Types.TriggerStore -> Types.Value -> Types.Memory -> Types.TriggerStore
addNewTrigger triggerStore closureValue x
  | Maybe.isNothing (Map.lookup x triggerStore) = Map.insert x [closureValue] triggerStore
  | otherwise = Map.insert x updatedValueList triggerStore
  where updatedValueList = (Maybe.fromJust (Map.lookup x triggerStore)) ++ [closureValue]
