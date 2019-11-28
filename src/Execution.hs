-- Module for handling functions based on Multi Stack and
-- Concurrency features, and the thread Scheduler + Context Switching
-- Part of Declarative Concurrent Model

-- Exposes only threadScheduler, all other functions are local
-- and not needed by anything outside (for now)
module Execution
  (threadScheduler) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Types
import qualified Helpers
import qualified SingleAssignmentStore as SAS
import qualified TriggerStore as TS
import qualified MutableStore as MS

-- Checks whether a variable is bound to a Value in the SAS or not
-- Returns False if the variable does not exist in the environment or
-- is not bound to a value.
isBound :: Types.Identifier -> Types.EnvironmentMap -> Types.SingleAssignmentStore -> Bool
isBound src env (eqMap, valueMap)
  | (Maybe.isJust $ Map.lookup src env) == True = if (Maybe.isJust eqClass)
                                                    then (Maybe.isJust $ Map.lookup (Maybe.fromJust eqClass) valueMap)
                                                    else False
  | otherwise = False
  where var = Map.lookup src env
        eqClass = Map.lookup (Maybe.fromJust var) eqMap


-- ####################################################################################################
-- Threading functions (for new threads and context switching between READY threads)
-- ####################################################################################################

-- threadScheduler takes a SAS, Remaining Memory List, List of Stack Tuples each denoting a thread (containing state and variable it is suspended on if any),
-- returns the updated SAS, Memory List and List of Stack Tuples (Always sorted according to the Stack State) according to the following 3 cases:
-- All Threads are in Completed State -> return since program execution is complere
-- All Threads are either in Suspended/Completed State (No Ready Thread) -> Give an error as program cannot be executed further.
-- Program contains ATLEAT ONE READY Thread -> performs a Context Switch to the thread, executes it and returns updated SAS, Memory and StackList which needs to be appended.
threadScheduler :: Types.SingleAssignmentStore -> Types.TriggerStore -> Types.MutableStore -> Types.MemoryList -> [(Types.Stack, Types.StackState, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Identifier)] -> (Types.SingleAssignmentStore, Types.TriggerStore, Types.MutableStore, Types.MemoryList, [(Types.Stack, Types.StackState, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Identifier)])
threadScheduler sas triggerStore mutableStore memory stackList
  | null stackList                                        = (sas, triggerStore, mutableStore, memory, stackList)
  | (_, Types.Completed, _, _, _, _, _) <- head stackList = (sas, triggerStore, mutableStore, memory, stackList)
  | (_, Types.Suspended, _, _, _, _, _) <- head stackList = error $ "All statements are in suspended state, cannot execute the threads!"
  | (_, Types.Ready, _, _, _, _, _)     <- head stackList = threadScheduler updatedSas updatedTriggerStore updatedMutableStore updatedMemory updatedStackList
  where (updatedSas, triggerStoreAfterExecution, updatedMutableStore, updatedMemory, stackListToAppend) = contextSwitchAndExecute sas triggerStore mutableStore memory stack envList sasList triggerStoreList mutableStoreList
        (stack, _, envList, sasList, triggerStoreList, mutableStoreList, _) = head stackList
        updatedStackList = List.sortOn (\(_,state,_,_,_,_,_) -> state) updatedStateStackList
        (updatedTriggerStore, updatedStateStackList) = updateSuspendedState updatedSas triggerStoreAfterExecution ((tail stackList) ++ stackListToAppend)

-- contextSwitchAndExecute takes a SAS, Memory List, Stack to Execute and executes the stack by calling the executeStack function
-- it returns the updated SAS, Memory List and Stack Tuples.
-- The four cases of guards are trivially seen here
contextSwitchAndExecute :: Types.SingleAssignmentStore -> Types.TriggerStore -> Types.MutableStore -> Types.MemoryList -> Types.Stack -> [Types.EnvironmentMap] -> [Types.SingleAssignmentStore] -> [Types.TriggerStore] -> [Types.MutableStore] -> (Types.SingleAssignmentStore, Types.TriggerStore, Types.MutableStore, Types.MemoryList, [(Types.Stack, Types.StackState, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Identifier)])
contextSwitchAndExecute sas triggerStore mutableStore memory stack envList sasList triggerStoreList mutableStoreList
  | (null updatedStack) && (null newStacksToAdd)       = (updatedSas, updatedTriggerStore, updatedMutableStore, updatedMemory, [(updatedStack, Types.Completed, updatedEnvList, updatedSasList, updatedTriggerStoreList, updatedMutableStoreList, "")])
  | (not $ null updatedStack) && (null newStacksToAdd) = (updatedSas, updatedTriggerStore, updatedMutableStore, updatedMemory, [(updatedStack, Types.Suspended, updatedEnvList, updatedSasList, updatedTriggerStoreList, updatedMutableStoreList, suspendedOn)])
  | (null updatedStack) && (not $ null newStacksToAdd) = (updatedSas, updatedTriggerStore, updatedMutableStore, updatedMemory, newStackTuples ++ [(updatedStack, Types.Completed, updatedEnvList, updatedSasList, updatedTriggerStoreList, updatedMutableStoreList, "")])
  | otherwise                                          = (updatedSas, updatedTriggerStore, updatedMutableStore, updatedMemory, newStackTuples ++ [(updatedStack, Types.Suspended, updatedEnvList, updatedSasList, updatedTriggerStoreList, updatedMutableStoreList, suspendedOn)])
  where (updatedSas, updatedTriggerStore, updatedMutableStore, updatedMemory, updatedEnvList, updatedSasList, updatedTriggerStoreList, updatedMutableStoreList, updatedStack, newStacksToAdd) = executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList stack []
        suspendedOn = getSuspendedVar (head updatedStack) updatedSas
        newStackTuples = map (\stack -> (stack, Types.Ready, [], [], [], [], "")) newStacksToAdd

-- ####################################################################################################


-- ####################################################################################################
-- Functions dealing with Cases of Suspended Statement (i.e. Suspended Threads)
-- ####################################################################################################

-- getSuspendedVar returns the Identifier on which the stack is suspended
getSuspendedVar :: Types.StackElement -> Types.SingleAssignmentStore -> Types.Identifier
getSuspendedVar ((Types.BindValue _ value), env) sas   = getSuspendedVarInExpression value env sas
getSuspendedVar ((Types.Conditional src _ _), env) sas = src
getSuspendedVar ((Types.Match src _ _ _), env) sas     = src
getSuspendedVar ((Types.Apply src _), env) sas         = src
getSuspendedVar _ _                                    = ""

getSuspendedVarInExpression :: Types.ValuesRead -> Types.EnvironmentMap -> Types.SingleAssignmentStore -> Types.Identifier
getSuspendedVarInExpression (Types.Expr exp) env sas
  | (Types.Exp _ l r) <- exp = if ((getSuspendedVarInExpression (Types.Expr l) env sas) == "") then (getSuspendedVarInExpression (Types.Expr r) env sas) else (getSuspendedVarInExpression (Types.Expr l) env sas)
  | (Types.Variable var) <- exp = if (isBound var env sas) then "" else var
  | otherwise = ""

-- The updateSuspendedState will update the Stack State of all the Stacks for which the variable (on which they were suspended) is not bound in the SAS,
-- It will only change the state of Suspended stacks, others are left as it is.
updateSuspendedState :: Types.SingleAssignmentStore -> Types.TriggerStore -> [(Types.Stack, Types.StackState, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Identifier)] -> (Types.TriggerStore, [(Types.Stack, Types.StackState, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Identifier)])
updateSuspendedState sas triggerStore stackList = (updatedTriggerStore, updatedStateStackList ++ newStackTuples)
  where updatedStateStackList = map (stateChangeFunc sas) stackList
        (updatedTriggerStore, newStackTuples) = TS.activateTriggers sas triggerStore listOfSuspendedAddresses
        listOfSuspendedAddresses = map (\(stack,_,_,_,_,_,var) -> Maybe.fromJust (Map.lookup var (snd $ head stack))) $ filter (\(_,state,_,_,_,_,_) -> state == Types.Suspended) updatedStateStackList

-- The stateChangeFunc is just a helper function for the Map used above,
-- The working is trivial, if State of Stack is Suspended and variable (on which it is suspended) is bound, then update it else do nothing.
stateChangeFunc :: Types.SingleAssignmentStore -> (Types.Stack, Types.StackState, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Identifier) -> (Types.Stack, Types.StackState, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Identifier)
stateChangeFunc sas (stack, state, envList, sasList, triggerStoreList, mutableStoreList, var)
  | state == Types.Suspended = if (isBound var (snd $ head stack) sas)
                                 then (stack, Types.Ready, envList, sasList, triggerStoreList, mutableStoreList, "")
                                 else (stack, state, envList, sasList, triggerStoreList, mutableStoreList, var)
  | otherwise                = (stack, state, envList, sasList, triggerStoreList, mutableStoreList, var)


-- ####################################################################################################

-- THE MAIN FUNCTION, TAKING CARE OF EACH STATEMENT INDIVIDUALLY

-- executeStack takes current single assignment store, memory, environment list (for output), semantic stack and
-- List of new stacks which need to be added to the semantic multi stack.
-- In each execution, it pops the top statement and calls execute stack with updated SAS, memory, environment list and semantic stack
-- If the the semantic stack produced is empty, then execution is successful and not suspended.
-- NOTE: The last return value in the Tuple is the stacks (which need to added to multi stack for thread statements)

executeStack :: Types.SingleAssignmentStore -> Types.TriggerStore -> Types.MutableStore -> Types.MemoryList -> [Types.EnvironmentMap] -> [Types.SingleAssignmentStore] -> [Types.TriggerStore] -> [Types.MutableStore] -> Types.Stack -> [Types.Stack] -> (Types.SingleAssignmentStore, Types.TriggerStore, Types.MutableStore, Types.MemoryList, [Types.EnvironmentMap], [Types.SingleAssignmentStore], [Types.TriggerStore], [Types.MutableStore], Types.Stack, [Types.Stack])
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList [] stacks = (sas, triggerStore, mutableStore, memory, envList, sasList, triggerStoreList, mutableStoreList, [], stacks)

-- Skip Statement
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList ((Types.Skip, env):xs) stacks = executeStack sas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) xs stacks

-- Multiple Statements
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.Multiple stmts), env):xs) stacks = executeStack sas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) (stmtList ++ xs) stacks
  where stmtList = map (\b -> (b, env)) stmts

-- Var Statements
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.Var dest stmt), env):xs) stacks = executeStack updatedSas triggerStore mutableStore updatedMemory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) ([stackElement] ++ xs) stacks
  where (updatedSas, updatedMemory) = SAS.addVariable sas memory
        updatedEnv                  = Map.insert dest (head memory) env
        stackElement                = (stmt, updatedEnv)

-- BindIdent Statements
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.BindIdent dest src), env):xs) stacks = executeStack updatedSas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) xs stacks
  where updatedSas = if (Maybe.isNothing (Map.lookup dest env)) || (Maybe.isNothing (Map.lookup src env))
                       then error $ "Bind Identifier Statement Error: Var " ++ dest ++ " OR Var " ++ src ++ " not in scope."
                       else SAS.unifyVariables sas x y
                         where x = Maybe.fromJust (Map.lookup dest env)
                               y = Maybe.fromJust (Map.lookup src env)

-- BindValue Statements
-- Suspend in case variable is not bound (for expression value)
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.BindValue dest value), env):xs) stacks
  | Maybe.isJust maybeUpdatedSas = executeStack updatedSas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) xs stacks
  | otherwise = (sas, triggerStore, mutableStore, memory, envList, sasList, triggerStoreList, mutableStoreList, (((Types.BindValue dest value), env):xs), stacks)
  where maybeUpdatedSas = SAS.bindValue sas (Maybe.fromJust (Map.lookup dest env)) value env
        updatedSas = if (Maybe.isNothing (Map.lookup dest env))
                       then error $ "Bind Value Statement Error: Var " ++ dest ++ " not in scope."
                       else Maybe.fromJust maybeUpdatedSas

-- Conditional Statement
-- do nothing with trigger store here, if suspended then will be taken care by updateStackState function
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.Conditional src fststmt sndstmt), env):xs) stacks
  | isBound src env sas == True = executeStack sas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) ([stackElement] ++ xs) stacks
  | Maybe.isNothing (Map.lookup src env) == True = error $ "Conditional Statement Error: Var " ++ src ++ " not in scope."
  | otherwise = (sas, triggerStore, mutableStore, memory, envList, sasList, triggerStoreList, mutableStoreList, (((Types.Conditional src fststmt sndstmt), env):xs), stacks)
  where stackElement = if (Helpers.isLit val)
                         then if (Types.litVal val) /= 0
                                then (fststmt, env)
                                else (sndstmt, env)
                         else error $ "The type of variable " ++ src ++ " should be a literal."
                         where val = Maybe.fromJust $ Helpers.getValue src env sas

-- Match Statement
-- do nothing with trigger store here, if suspended then will be taken care by updateStackState function
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.Match src pattern fststmt sndstmt), env):xs) stacks
  | isBound src env sas == True = executeStack sas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) ([stackElement] ++ xs) stacks
  | Maybe.isNothing (Map.lookup src env) == True = error $ "Match Statement Error: Var " ++ src ++ " not in scope."
  | otherwise = (sas, triggerStore, mutableStore, memory, envList, sasList, triggerStoreList, mutableStoreList, (((Types.Match src pattern fststmt sndstmt), env):xs), stacks)
  where stackElement = if (Helpers.isRec val) && (Helpers.isRecord pattern)
                         then if (Helpers.matchPattern val pattern) -- Match label, arity and features
                                then (fststmt, Helpers.extendEnvFromPattern val pattern env)
                                else (sndstmt, env)
                         else error $ "The type of variables " ++ src ++ " and Pattern: " ++ (show pattern) ++ " should be record."
                         where val = Maybe.fromJust $ Helpers.getValue src env sas

-- Apply Statement (Procedure Application)
-- do nothing with trigger store here, if suspended then will be taken care by updateStackState function
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.Apply func parameters), env):xs) stacks
  | isBound func env sas == True = executeStack sas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) ([stackElement] ++ xs) stacks
  | Maybe.isNothing (Map.lookup func env) == True = error $ "Apply Statement Error: Var " ++ func ++ " not in scope."
  | otherwise = (sas, triggerStore, mutableStore, memory, envList, sasList, triggerStoreList, mutableStoreList, (((Types.Apply func parameters), env):xs), stacks)
  where stackElement = if (Helpers.isClosure val)
                         then if (length parameters) == (length $ Types.procParameters val)
                                then (Types.procStmt val, Helpers.extendEnvFromClosure val parameters env)
                                else error $ "The function/procedure call " ++ func ++ " lists " ++ (show $ length parameters) ++
                                     " parameters but closure lists " ++ (show $ length $ Types.procParameters val) ++ " parameters."
                         else error $ "The type of variable " ++ func ++ " should be a closure/procedure."
                         where val = Maybe.fromJust $ Helpers.getValue func env sas

-- Thread Statement (Adding a new Stack, for multi stack)
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.Thread stmt), env):xs) stacks = executeStack sas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) xs (stacks ++ [[(stmt, env)]])

-- ByNeed Statement (Adding a trigger or a new stack depending on variable being bound)
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.ByNeed dest value), env):xs) stacks
  | not (Helpers.isProc value) || (length $ Types.params value) /= 1 = error $ "ByNeed Statement: The value provided: " ++ (show value) ++ " is not a valid one-argument procedure."
  | isBound dest env sas = executeStack sas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) xs updatedStackList
  | otherwise            = executeStack sas updatedTriggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) xs stacks
  where closureValue = Maybe.fromJust $ Helpers.convertValuesReadToValue value env sas
        updatedStackList = stacks ++ [[(Types.procStmt closureValue, Helpers.extendEnvFromClosure closureValue [dest] env)]]
        updatedTriggerStore = TS.addNewTrigger triggerStore closureValue (Maybe.fromJust (Map.lookup dest env))

-- NewPort Statement (Creating a new port for receiving messages, part of Message Passing Model)
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.NewPort src dest), env):xs) stacks
  | Maybe.isNothing (Map.lookup src env)  = error $ "NewPort Statement Error: Var " ++ src ++ " not in scope."
  | Maybe.isNothing (Map.lookup dest env) = error $ "NewPort Statement Error: Var " ++ dest ++ " not in scope."
  | otherwise = executeStack updatedSas triggerStore updatedMutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) xs stacks
  where (updatedSas, updatedMutableStore) = MS.addNewPort sas mutableStore streamAddress portAddress
        streamAddress = Maybe.fromJust $ Map.lookup src env
        portAddress   = Maybe.fromJust $ Map.lookup dest env

-- Send Statement (Sending some data to a port)
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.Send dest msg), env):xs) stacks
  | Maybe.isNothing (Map.lookup dest env) = error $ "Send Statement Error: Var " ++ dest ++ " not in scope."
  | Maybe.isNothing (Map.lookup msg env)  = error $ "Send Statement Error: Var " ++ msg ++ " not in scope."
  | Maybe.isNothing (Map.lookup portEqClass (snd sas)) = error $ "Send Statement Error: Port " ++ dest ++ " not created, please use NewPort Statement."
  | Maybe.isNothing (Map.lookup portId mutableStore) = error $ "Send Statement Error: Port " ++ dest ++ " not bound to a valid stream!"
  | otherwise = executeStack updatedSas triggerStore updatedMutableStore updatedMemory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) xs stacks
  where (updatedSas, updatedMutableStore, updatedMemory) = MS.sendDataToPort sas mutableStore memory portId msgAddr
        portEqClass = Maybe.fromJust $ Map.lookup (Maybe.fromJust $ Map.lookup dest env) (fst sas)
        portId = Types.name $ Maybe.fromJust $ Map.lookup portEqClass (snd sas)
        msgAddr = Maybe.fromJust $ Map.lookup msg env

-- Wait Statement (Waiting for the variable to be bound.)
executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList (((Types.Wait src), env):xs) stacks
  | isBound src env sas = executeStack sas triggerStore mutableStore memory (envList ++ [env]) (sasList ++ [sas]) (triggerStoreList ++ [triggerStore]) (mutableStoreList ++ [mutableStore]) xs stacks
  | Maybe.isNothing (Map.lookup src env) = error $ "Wait Statement Error: Var " ++ src ++ " not in scope."
  | otherwise = (sas, triggerStore, mutableStore, memory, envList, sasList, triggerStoreList, mutableStoreList, (((Types.Wait src), env):xs), stacks)

-- Error Case (Redundant for now)
-- executeStack sas triggerStore mutableStore memory envList sasList triggerStoreList mutableStoreList stack stacks = (sas, triggerStore, mutableStore, memory, envList, sasList, triggerStoreList, mutableStoreList, stack, stacks)
