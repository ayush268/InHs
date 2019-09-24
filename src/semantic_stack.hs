-- Module for the Semantic Stack object and its methods

--module semantic_stack
--  (executeStack) where

import qualified Types

import qualified Data.Map as Map
import qualified Data.UUID as UUID
import qualified System.Random as Rand

executeStack :: Types.MemoryToEqClassMap -> [(Types.Statement, Types.EnvironmentMap)] -> (Bool, Types.MemoryToEqClassMap)

executeStack sas = foldl foldingFunction (True, sas)
  where foldingFunction (result, sas) (Types.Skip, env) = ((True && result), sas)
        foldingFunction (result, sas) ((Types.Multiple x), env) = ((executionResult && result), executionSas)
          where (executionResult, executionSas) = executeStack sas (map (\b -> (b, env)) x)
        foldingFunction (result, sas) _ = (False, sas)

executeProgram :: Types.Statement -> (Bool, Types.MemoryToEqClassMap)
executeProgram x = executeStack Map.empty [(x, Map.empty)]
