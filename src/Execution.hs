-- Module for the Semantic Stack object and its methods

--module Execution
--  (executeProgram) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.UUID as UUID
import qualified System.Random as Rand

import qualified Types
import qualified SingleAssignmentStore as SAS

-- executeStack takes current single assignment store and semantic stack
-- After each execution it produces a tuple denoting successful execution and 
-- updated Single Assignment Store
executeStack :: Types.SingleAssignmentStore -> Types.MemoryList -> [(Types.Statement, Types.EnvironmentMap)] -> (Bool, Types.SingleAssignmentStore, Types.MemoryList, [Types.EnvironmentMap])
executeStack sas memory = foldl foldingFunction (True, sas, memory, [])

-- Skip Statement
  where foldingFunction (result, sas, memory, envList) (Types.Skip, env) = ((True && result), sas, memory, envList ++ [env])

-- Multiple Statements
        foldingFunction (result, sas, memory, envList) ((Types.Multiple stmts), env) = ((executionResult && result), executionSas, executionMemory, envList ++ executionEnv)
          where (executionResult, executionSas, executionMemory, executionEnv) = executeStack sas memory (map (\b -> (b, env)) stmts)

-- Var Statements
        foldingFunction (result, sas, memory, envList) ((Types.Var dest stmt), env) = ((executionResult && result), executionSas, executionMemory, envList ++ [env] ++ executionEnv)
          where (executionResult, executionSas, executionMemory, executionEnv) = executeStack updatedSas updatedMemory [(stmt, updatedEnv)]
                  where (updatedSas, updatedMemory) = SAS.addVariable sas memory
                        updatedEnv                  = Map.insert dest (head memory) env

-- BindIdent Statements
        foldingFunction (result, sas, memory, envList) ((Types.BindIdent dest src), env) = (result, executionSas, memory, envList ++ [env])
          where executionSas = if (Maybe.isNothing (Map.lookup dest env)) || (Maybe.isNothing (Map.lookup src env))
                                then error $ "Bind Identifier Statement Error: Var " ++ dest ++ " OR Var " ++ src ++ " not in scope."
                                else SAS.unifyVariables sas x y
                                  where x = Maybe.fromJust (Map.lookup dest env)
                                        y = Maybe.fromJust (Map.lookup src env)

-- Error Case
        foldingFunction (result, sas, memory, envList) _ = (False, sas, memory, envList)


-- Actual Program Execution
executeProgram :: Types.Statement -> (Bool, Types.SingleAssignmentStore, Types.MemoryList, [Types.EnvironmentMap])
executeProgram x = executeStack (Map.empty, Map.empty) [1..] [(x, Map.empty)]
