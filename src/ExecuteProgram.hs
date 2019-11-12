-- Module for importing relevant module for calling the thread Scheduler
-- exposes a function which takes a statement (Complete Program) as input
-- and executes it,
-- The program can consist of Multiple Threads (Concurrency is Supported)

module ExecuteProgram
  (executeProgram) where

import qualified Data.Map as Map

import qualified Types
import qualified Execution

-- Actual Program Execution
-- Returns the Final state of the SingleAssignmentStore along with
-- The Sequence of Environments for each thread which was created separately (as a list)
executeProgram :: Types.Statement -> (Types.SingleAssignmentStore, Types.TriggerStore, [[Types.EnvironmentMap]], [[Types.SingleAssignmentStore]], [[Types.TriggerStore]])
executeProgram x = (sas, triggerStore, listOfEnvLists, listOfSas, listOfTriggerStores)
  where (sas, triggerStore, _, stackTuples) = Execution.threadScheduler (Map.empty, Map.empty) Map.empty [1..] [([(x, Map.empty)], Types.Ready, [], [], [], "")]
        listOfEnvLists        = map (\(_, _, envList, _, _, _) -> envList) stackTuples
        listOfSas             = map (\(_, _, _, sasList, _, _) -> sasList) stackTuples
        listOfTriggerStores   = map (\(_, _, _, _, triggerStoreList, _) -> triggerStoreList) stackTuples
