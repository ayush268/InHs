-- Module for the Semantic Stack object and its methods

--module Execution
--  (executeProgram) where

import qualified Data.Map as Map
import qualified Data.UUID as UUID
import qualified System.Random as Rand

import qualified Types
import qualified SingleAssignmentStore as SAS

-- executeStack takes current single assignment store and semantic stack
-- After each execution it produces a tuple denoting successful execution and 
-- updated Single Assignment Store
executeStack :: Types.SingleAssignmentStore -> Types.MemoryList -> [(Types.Statement, Types.EnvironmentMap)] -> (Bool, Types.SingleAssignmentStore, Types.MemoryList)
executeStack sas memory = foldl foldingFunction (True, sas, memory)

-- Skip Statement
  where foldingFunction (result, sas, memory) (Types.Skip, env) = ((True && result), sas, memory)

-- Multiple Statements
        foldingFunction (result, sas, memory) ((Types.Multiple stmts), env) = ((executionResult && result), executionSas, executionMemory)
          where (executionResult, executionSas, executionMemory) = executeStack sas memory (map (\b -> (b, env)) stmts)

-- Var Statements
        foldingFunction (result, sas, memory) ((Types.Var dest stmt), env) = ((executionResult && result), executionSas, executionMemory)
          where (executionResult, executionSas, executionMemory) = executeStack updatedSas updatedMemory [(stmt, updatedEnv)]
                  where (updatedSas, updatedMemory) = SAS.addVariable sas memory
                        updatedEnv                  = Map.insert dest (head memory) env

-- Error Case
        foldingFunction (result, sas, memory) _ = (False, sas, memory)


-- Actual Program Execution
executeProgram :: Types.Statement -> (Bool, Types.SingleAssignmentStore, Types.MemoryList)
executeProgram x = executeStack (Map.empty, Map.empty) [1..] [(x, Map.empty)]
