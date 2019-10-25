-- Module for the Semantic Stack object and its methods

module ExecuteProgram
  (executeProgram) where

import qualified Data.Map as Map

import qualified Types
import qualified Execution

-- Actual Program Execution
executeProgram :: Types.Statement -> (Types.SingleAssignmentStore, [[Types.EnvironmentMap]])
executeProgram x = (sas, listOfEnvLists)
  where (sas, _, stackTuples) = Execution.threadScheduler (Map.empty, Map.empty) [1..] [([(x, Map.empty)], Types.Ready, [], "")]
        listOfEnvLists        = map (\(_, _, envList, _) -> envList) stackTuples
