-- Module for handling functions based on Multi Stack and
-- Concurrency features, and the thread Scheduler + Context Switching
-- Part of Declarative Concurrent Model

module Execution
  (threadScheduler) where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Types
import qualified Helpers
import qualified SingleAssignmentStore as SAS

isBound :: Types.Identifier -> Types.EnvironmentMap -> Types.SingleAssignmentStore -> Bool
isBound src env (eqMap, valueMap)
  | (Maybe.isJust $ Map.lookup src env) == True = if (Maybe.isJust eqClass)
                                                    then (Maybe.isJust $ Map.lookup (Maybe.fromJust eqClass) valueMap)
                                                    else False
  | otherwise = False
  where var = Map.lookup src env
        eqClass = Map.lookup (Maybe.fromJust var) eqMap

threadScheduler :: Types.SingleAssignmentStore -> Types.MemoryList -> [(Types.Stack, Types.StackState, [Types.EnvironmentMap], Types.Identifier)] -> (Types.SingleAssignmentStore, Types.MemoryList, [(Types.Stack, Types.StackState, [Types.EnvironmentMap], Types.Identifier)])
threadScheduler sas memory stackList
  | null stackList                               = (sas, memory, stackList)
  | (_, Types.Completed, _, _) <- head stackList = (sas, memory, stackList)
  | (_, Types.Suspended, _, _) <- head stackList = error $ "All statements are in suspended state, cannot execute the threads!"
  | (_, Types.Ready, _, _)     <- head stackList = threadScheduler updatedSas updatedMemory updatedStackList
  where (updatedSas, updatedMemory, stackListToAppend) = contextSwitchAndExecute sas memory stack envList
        (stack, _, envList, _) = head stackList
        updatedStackList = List.sortOn (\(_,state,_,_) -> state) updatedStateStackList
        updatedStateStackList = updateSuspendedState updatedSas ((tail stackList) ++ stackListToAppend)

contextSwitchAndExecute :: Types.SingleAssignmentStore -> Types.MemoryList -> Types.Stack -> [Types.EnvironmentMap] -> (Types.SingleAssignmentStore, Types.MemoryList, [(Types.Stack, Types.StackState, [Types.EnvironmentMap], Types.Identifier)])
contextSwitchAndExecute sas memory stack envList
  | (null updatedStack) && (null newStacksToAdd)       = (updatedSas, updatedMemory, [(updatedStack, Types.Completed, updatedEnvList, "")])
  | (not $ null updatedStack) && (null newStacksToAdd) = (updatedSas, updatedMemory, [(updatedStack, Types.Suspended, updatedEnvList, suspendedOn)])
  | (null updatedStack) && (not $ null newStacksToAdd) = (updatedSas, updatedMemory, newStackTuples ++ [(updatedStack, Types.Completed, updatedEnvList, "")])
  | otherwise                                          = (updatedSas, updatedMemory, newStackTuples ++ [(updatedStack, Types.Suspended, updatedEnvList, suspendedOn)])
  where (updatedSas, updatedMemory, updatedEnvList, updatedStack, newStacksToAdd) = executeStack sas memory envList stack []
        suspendedOn = getSuspendedVar $ head updatedStack
        newStackTuples = map (\stack -> (stack, Types.Ready, [], "")) newStacksToAdd


getSuspendedVar :: Types.StackElement -> Types.Identifier
getSuspendedVar ((Types.Conditional src _ _), env) = src
getSuspendedVar ((Types.Match src _ _ _), env)     = src
getSuspendedVar ((Types.Apply src _), env)         = src
getSuspendedVar _                                  = ""


updateSuspendedState :: Types.SingleAssignmentStore -> [(Types.Stack, Types.StackState, [Types.EnvironmentMap], Types.Identifier)] -> [(Types.Stack, Types.StackState, [Types.EnvironmentMap], Types.Identifier)]
updateSuspendedState sas stackList = map (stateChangeFunc sas) stackList


stateChangeFunc :: Types.SingleAssignmentStore -> (Types.Stack, Types.StackState, [Types.EnvironmentMap], Types.Identifier) -> (Types.Stack, Types.StackState, [Types.EnvironmentMap], Types.Identifier)
stateChangeFunc sas (stack, state, envList, var)
  | state == Types.Suspended = if (isBound var (snd $ head stack) sas)
                                 then (stack, Types.Ready, envList, "")
                                 else (stack, state, envList, var)
  | otherwise                = (stack, state, envList, var)

-- executeStack takes current single assignment store, memory, environment list (for output), semantic stack and
-- List of new stacks which need to be added to the semantic multi stack.
-- In each execution, it pops the top statement and calls execute stack with updated SAS, memory, environment list and semantic stack
-- If the the semantic stack produced is empty, then execution is successful and not suspended.
-- NOTE: The last return value in the Tuple is the stacks (which need to added to multi stack for thread statements)

executeStack :: Types.SingleAssignmentStore -> Types.MemoryList -> [Types.EnvironmentMap] -> Types.Stack -> [Types.Stack] -> (Types.SingleAssignmentStore, Types.MemoryList, [Types.EnvironmentMap], Types.Stack, [Types.Stack])
executeStack sas memory envList [] stacks = (sas, memory, envList, [], stacks)

-- Skip Statement
executeStack sas memory envList ((Types.Skip, env):xs) stacks = executeStack sas memory (envList ++ [env]) xs stacks

-- Multiple Statements
executeStack sas memory envList (((Types.Multiple stmts), env):xs) stacks = executeStack sas memory (envList ++ [env]) (stmtList ++ xs) stacks
  where stmtList = map (\b -> (b, env)) stmts

-- Var Statements
executeStack sas memory envList (((Types.Var dest stmt), env):xs) stacks = executeStack updatedSas updatedMemory (envList ++ [env]) ([stackElement] ++ xs) stacks
  where (updatedSas, updatedMemory) = SAS.addVariable sas memory
        updatedEnv                  = Map.insert dest (head memory) env
        stackElement                = (stmt, updatedEnv)

-- BindIdent Statements
executeStack sas memory envList (((Types.BindIdent dest src), env):xs) stacks = executeStack updatedSas memory (envList ++ [env]) xs stacks
  where updatedSas = if (Maybe.isNothing (Map.lookup dest env)) || (Maybe.isNothing (Map.lookup src env))
                       then error $ "Bind Identifier Statement Error: Var " ++ dest ++ " OR Var " ++ src ++ " not in scope."
                       else SAS.unifyVariables sas x y
                         where x = Maybe.fromJust (Map.lookup dest env)
                               y = Maybe.fromJust (Map.lookup src env)

-- BindValue Statements
executeStack sas memory envList (((Types.BindValue dest value), env):xs) stacks = executeStack updatedSas memory (envList ++ [env]) xs stacks
  where updatedSas = if (Maybe.isNothing (Map.lookup dest env))
                       then error $ "Bind Value Statement Error: Var " ++ dest ++ " not in scope."
                       else SAS.bindValue sas x value env
                         where x = Maybe.fromJust (Map.lookup dest env)

-- Conditional Statement
executeStack sas memory envList (((Types.Conditional src fststmt sndstmt), env):xs) stacks
  | isBound src env sas == True = executeStack sas memory (envList ++ [env]) ([stackElement] ++ xs) stacks
  | Maybe.isNothing (Map.lookup src env) == True = error $ "Conditional Statement Error: Var " ++ src ++ " not in scope."
  | otherwise = (sas, memory, envList, (((Types.Conditional src fststmt sndstmt), env):xs), stacks)
  where stackElement = if (Helpers.isLit val)
                         then if (Types.litVal val) /= 0
                                then (fststmt, env)
                                else (sndstmt, env)
                         else error $ "The type of variable " ++ src ++ " should be a literal."
                         where val = Helpers.getValue src env sas

-- Match Statement
executeStack sas memory envList (((Types.Match src pattern fststmt sndstmt), env):xs) stacks
  | isBound src env sas == True = executeStack sas memory (envList ++ [env]) ([stackElement] ++ xs) stacks
  | Maybe.isNothing (Map.lookup src env) == True = error $ "Match Statement Error: Var " ++ src ++ " not in scope."
  | otherwise = (sas, memory, envList, (((Types.Match src pattern fststmt sndstmt), env):xs), stacks)
  where stackElement = if (Helpers.isRec val) && (Helpers.isRecord pattern)
                         then if (Helpers.matchPattern val pattern) -- Match label, arity and features
                                then (fststmt, Helpers.extendEnvFromPattern val pattern env)
                                else (sndstmt, env)
                         else error $ "The type of variables " ++ src ++ " and Pattern: " ++ (show pattern) ++ " should be record."
                         where val = Helpers.getValue src env sas

-- Apply Statement (Procedure Application)
executeStack sas memory envList (((Types.Apply func parameters), env):xs) stacks
  | isBound func env sas == True = executeStack sas memory (envList ++ [env]) ([stackElement] ++ xs) stacks
  | Maybe.isNothing (Map.lookup func env) == True = error $ "Apply Statement Error: Var " ++ func ++ " not in scope."
  | otherwise = (sas, memory, envList, (((Types.Apply func parameters), env):xs), stacks)
  where stackElement = if (Helpers.isClosure val)
                         then if (length parameters) == (length $ Types.procParameters val)
                                then (Types.procStmt val, Helpers.extendEnvFromClosure val parameters env)
                                else error $ "The function/procedure call " ++ func ++ " lists " ++ (show $ length parameters) ++
                                     " parameters but closure lists " ++ (show $ length $ Types.procParameters val) ++ " parameters."
                         else error $ "The type of variable " ++ func ++ " should be a closure/procedure."
                         where val = Helpers.getValue func env sas

-- Thread Statement (Adding a new Stack, for multi stack)
executeStack sas memory envList (((Types.Thread stmt), env):xs) stacks = executeStack sas memory envList xs (stacks ++ [[(stmt, env)]])

-- Error Case (Redundant for now)
-- executeStack sas memory envList stack stacks = (sas, memory, envList, stack, stacks)
