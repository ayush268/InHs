-- Module for the Semantic Stack object and its methods

module Execution
  (executeProgram) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Types
import qualified Helpers
import qualified SingleAssignmentStore as SAS


-- executeStack takes current single assignment store, memory and environment list (for output) and semantic stack
-- In each execution, it pops the top statement and calls execute stack with updated SAS, memory, environment list and semantic stack
-- If the the semantic stack produced is empty, then execution is successful and not suspended.
executeStack :: Types.SingleAssignmentStore -> Types.MemoryList -> [Types.EnvironmentMap] -> Types.Stack -> (Types.SingleAssignmentStore, Types.MemoryList, [Types.EnvironmentMap], Types.Stack)
executeStack sas memory envList [] = (sas, memory, envList, [])

-- Skip Statement
executeStack sas memory envList ((Types.Skip, env):xs) = executeStack sas memory (envList ++ [env]) xs

-- Multiple Statements
executeStack sas memory envList (((Types.Multiple stmts), env):xs) = executeStack sas memory (envList ++ [env]) (stmtList ++ xs)
  where stmtList = map (\b -> (b, env)) stmts

-- Var Statements
executeStack sas memory envList (((Types.Var dest stmt), env):xs) = executeStack updatedSas updatedMemory (envList ++ [env]) ([stackElement] ++ xs)
  where (updatedSas, updatedMemory) = SAS.addVariable sas memory
        updatedEnv                  = Map.insert dest (head memory) env
        stackElement                = (stmt, updatedEnv)

-- BindIdent Statements
executeStack sas memory envList (((Types.BindIdent dest src), env):xs) = executeStack updatedSas memory (envList ++ [env]) xs
  where updatedSas = if (Maybe.isNothing (Map.lookup dest env)) || (Maybe.isNothing (Map.lookup src env))
                       then error $ "Bind Identifier Statement Error: Var " ++ dest ++ " OR Var " ++ src ++ " not in scope."
                       else SAS.unifyVariables sas x y
                         where x = Maybe.fromJust (Map.lookup dest env)
                               y = Maybe.fromJust (Map.lookup src env)

-- BindValue Statements
executeStack sas memory envList (((Types.BindValue dest value), env):xs) = executeStack updatedSas memory (envList ++ [env]) xs
  where updatedSas = if (Maybe.isNothing (Map.lookup dest env))
                       then error $ "Bind Value Statement Error: Var " ++ dest ++ " not in scope."
                       else SAS.bindValue sas x value env
                         where x = Maybe.fromJust (Map.lookup dest env)

-- Conditional Statement
executeStack sas memory envList (((Types.Conditional src fststmt sndstmt), env):xs) = executeStack sas memory (envList ++ [env]) ([stackElement] ++ xs)
  where stackElement = if (Helpers.isLit val)
                         then if (Types.litVal val) /= 0
                                then (fststmt, env)
                                else (sndstmt, env)
                         else error $ "The type of variable " ++ src ++ " should be a literal."
                         where val = Helpers.getValue src env sas

-- Match Statement
executeStack sas memory envList (((Types.Match src pattern fststmt sndstmt), env):xs) = executeStack sas memory (envList ++ [env]) ([stackElement] ++ xs)
  where stackElement = if (Helpers.isRec val) && (Helpers.isRecord pattern)
                         then if (Helpers.matchPattern val pattern) -- Match label, arity and features
                                then (fststmt, Helpers.extendEnvFromPattern val pattern env)
                                else (sndstmt, env)
                         else error $ "The type of variables " ++ src ++ " and Pattern: " ++ (show pattern) ++ " should be record."
                         where val = Helpers.getValue src env sas

-- Apply Statement (Procedure Application)
executeStack sas memory envList (((Types.Apply func parameters), env):xs) = executeStack sas memory (envList ++ [env]) ([stackElement] ++ xs)
  where stackElement = if (Helpers.isClosure val)
                         then if (length parameters) == (length $ Types.procParameters val)
                                then (Types.procStmt val, Helpers.extendEnvFromClosure val parameters env)
                                else error $ "The function/procedure call " ++ func ++ " lists " ++ (show $ length parameters) ++
                                     " parameters but closure lists " ++ (show $ length $ Types.procParameters val) ++ " parameters."
                         else error $ "The type of variable " ++ func ++ " should be a closure/procedure."
                         where val = Helpers.getValue func env sas

-- Error Case (Redundant for now)
-- executeStack sas memory envList stack = (sas, memory, envList, stack)


-- Actual Program Execution
executeProgram :: Types.Statement -> (Types.SingleAssignmentStore, Types.MemoryList, [Types.EnvironmentMap], Types.Stack)
executeProgram x = executeStack (Map.empty, Map.empty) [1..] [] [(x, Map.empty)]
