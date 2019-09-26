-- Module for the Semantic Stack object and its methods

module Execution
  (executeProgram) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.UUID as UUID
import qualified System.Random as Rand

import qualified Types
import qualified SingleAssignmentStore as SAS

-- executeStack takes current single assignment store and semantic stack
-- After each execution it produces a tuple denoting successful execution and 
-- updated Single Assignment Store
executeStack :: Types.SingleAssignmentStore -> Types.MemoryList -> [(Types.Statement, Types.EnvironmentMap)] -> (Types.SingleAssignmentStore, Types.MemoryList, [Types.EnvironmentMap])
executeStack sas memory = foldl foldingFunction (sas, memory, [])

-- Skip Statement
  where foldingFunction (sas, memory, envList) (Types.Skip, env) = (sas, memory, envList ++ [env])

-- Multiple Statements
        foldingFunction (sas, memory, envList) ((Types.Multiple stmts), env) = (executionSas, executionMemory, envList ++ executionEnv)
          where (executionSas, executionMemory, executionEnv) = executeStack sas memory (map (\b -> (b, env)) stmts)

-- Var Statements
        foldingFunction (sas, memory, envList) ((Types.Var dest stmt), env) = (executionSas, executionMemory, envList ++ [env] ++ executionEnv)
          where (executionSas, executionMemory, executionEnv) = executeStack updatedSas updatedMemory [(stmt, updatedEnv)]
                  where (updatedSas, updatedMemory) = SAS.addVariable sas memory
                        updatedEnv                  = Map.insert dest (head memory) env

-- BindIdent Statements
        foldingFunction (sas, memory, envList) ((Types.BindIdent dest src), env) = (executionSas, memory, envList ++ [env])
          where executionSas = if (Maybe.isNothing (Map.lookup dest env)) || (Maybe.isNothing (Map.lookup src env))
                                then error $ "Bind Identifier Statement Error: Var " ++ dest ++ " OR Var " ++ src ++ " not in scope."
                                else SAS.unifyVariables sas x y
                                  where x = Maybe.fromJust (Map.lookup dest env)
                                        y = Maybe.fromJust (Map.lookup src env)

-- BindValue Statements
        foldingFunction (sas, memory, envList) ((Types.BindValue dest value), env) = (executionSas, memory, envList ++ [env])
          where executionSas = if (Maybe.isNothing (Map.lookup dest env))
                                then error $ "Bind Value Statement Error: Var " ++ dest ++ " not in scope."
                                else SAS.bindValue sas x value env
                                  where x = Maybe.fromJust (Map.lookup dest env)

-- Error Case
        foldingFunction (sas, memory, envList) _ = (sas, memory, envList)


-- Actual Program Execution
executeProgram :: Types.Statement -> (Types.SingleAssignmentStore, Types.MemoryList, [Types.EnvironmentMap])
executeProgram x = executeStack (Map.empty, Map.empty) [1..] [(x, Map.empty)]
