-- Module for managing the Mutable Store
-- for the Interpreter

module MutableStore
  (addNewPort,
   sendDataToPort) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.UUID as Uuid

import qualified System.Random as Rand

import qualified Types
import qualified SingleAssignmentStore as SAS

addNewPort :: Types.SingleAssignmentStore -> Types.MutableStore -> Types.Memory -> Types.Memory -> (Types.SingleAssignmentStore, Types.MutableStore)
addNewPort (eqMap, valueMap) mutableStore stream port = (updatedSas, updatedMutableStore)
  where updatedMutableStore = Map.insert portId stream mutableStore
        portId = fst $ Rand.random $ Rand.mkStdGen port :: Uuid.UUID
        updatedSas = (eqMap, Map.insert portEqClass (Types.Name portId) valueMap)
        portEqClass = Maybe.fromJust $ Map.lookup port eqMap

sendDataToPort :: Types.SingleAssignmentStore -> Types.MutableStore -> [Types.Memory] -> Uuid.UUID -> Types.Memory -> (Types.SingleAssignmentStore, Types.MutableStore, [Types.Memory])
sendDataToPort (eqMap, valueMap) mutableStore memory portId msgAddr = (updatedSas, updatedMutableStore, updatedMemory)
  where newVariable = head memory
        (sasAfterNewVariable, updatedMemory) = SAS.addVariable (eqMap, valueMap) memory
        newRecord = Types.Rec 0 $ Map.fromList [(1, msgAddr), (2, newVariable)]
        updatedSas = bindMsgRecord sasAfterNewVariable (Maybe.fromJust $ Map.lookup portId mutableStore) newRecord
        updatedMutableStore = Map.insert portId newVariable mutableStore

bindMsgRecord :: Types.SingleAssignmentStore -> Types.Memory -> Types.Value -> Types.SingleAssignmentStore
bindMsgRecord (eqMap, valueMap) var record
  | Maybe.isNothing (Map.lookup eqClass valueMap) = (eqMap, Map.insert eqClass record valueMap)
  | otherwise = SAS.unifyValue (eqMap, valueMap) var record
  where eqClass = Maybe.fromJust $ Map.lookup var eqMap
