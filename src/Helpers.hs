-- Module for helper functions

module Helpers
  (isLit,
   isRec,
   isClosure,
   isRecord,
   isProc,
   getValue,
   matchPattern,
   extendEnvFromPattern,
   extendEnvFromClosure,
   updateOldEquivalenceClass,
   convertValuesReadToValue) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import qualified Types

-- ####################################################################################################
-- Functions which Takes Values and inform its type
-- ####################################################################################################

-- isLit informs whether a Value is literal (integer) or not
isLit :: Types.Value -> Bool
isLit (Types.Liter _) = True
isLit _ = False

-- isRec informs whether a Value is Record or not
isRec :: Types.Value -> Bool
isRec (Types.Rec _ _) = True
isRec _ = False

-- isClosure informs whether a Value is a Closure or not
isClosure :: Types.Value -> Bool
isClosure (Types.Closure _ _ _) = True
isClosure _ = False

-- Special for ValuesRead Type, 
-- isRecord informs whether a Read value is a Record
isRecord :: Types.ValuesRead -> Bool
isRecord (Types.Record _ _) = True
isRecord _ = False

-- Special for ValuesRead Type, 
-- isRecord informs whether a Read value is a Record
isProc :: Types.ValuesRead -> Bool
isProc (Types.Proc _ _) = True
isProc _ = False

-- ####################################################################################################


-- ####################################################################################################
-- Functions which Takes an Identifier and returns its Value (From SAS)
-- ####################################################################################################

-- getValue checks if the identifier is bound in SAS or not,
-- If its bound, then returns its value else raises an Exception
getValue :: Types.Identifier -> Types.EnvironmentMap -> Types.SingleAssignmentStore -> Types.Value
getValue src env (eqMap, valueMap) = if (Maybe.isJust var) && (Maybe.isJust eqClass) && (Maybe.isJust val)
                                       then (Maybe.fromJust val)
                                       else error $ "Variable " ++ src ++ " either not in scope or not bound to a value."
  where var = Map.lookup src env
        eqClass = Map.lookup (Maybe.fromJust var) eqMap
        val = Map.lookup (Maybe.fromJust eqClass) valueMap

-- ####################################################################################################


-- ####################################################################################################
-- Function which matches a stored Record with a read Pattern (Record)
-- ####################################################################################################

-- matchPattern takes a value (Record) and checks if it matches with,
-- pattern (record).
matchPattern :: Types.Value -> Types.ValuesRead -> Bool
matchPattern (Types.Rec a b) (Types.Record c d) = labelMatch && arityMatch && featuresMatch
  where labelMatch = (a == c)
        arityMatch = ((length b) == (length d))
        featuresMatch = ((length $ Set.union keysB keysD) == (length $ Set.intersection keysB keysD))
          where keysB = Set.fromList $ Map.keys b
                keysD = Set.fromList $ Map.keys d

matchPattern _ _ = False

-- ####################################################################################################


-- ####################################################################################################
-- Functions extending a given env based on the input value
-- ####################################################################################################

-- extendEnvFromPattern extends the input env based on the input Record and pattern
extendEnvFromPattern :: Types.Value -> Types.ValuesRead -> Types.EnvironmentMap -> Types.EnvironmentMap
extendEnvFromPattern (Types.Rec a val) (Types.Record b pattern) env = Map.union patternEnvMap env  -- Map union is left biased
  where patternEnvMap = Map.fromList $ zip patternIdents valMemories
          where patternIdents = Map.elems pattern
                valMemories = Map.elems val

extendEnvFromPattern _ _ env = env

-- extendEnvFromClosure extends the input env based on the input Closure and params
extendEnvFromClosure :: Types.Value -> [Types.Identifier] -> Types.EnvironmentMap -> Types.EnvironmentMap
extendEnvFromClosure (Types.Closure closureParams _ closureEnv) params env = Map.union closureEnvMap closureEnv -- Map union is left biased
  where closureEnvMap = Map.fromList $ zip closureParams memoryOfParams
          where memoryOfParams = map (\x -> Maybe.fromJust $ Map.lookup x env) params

extendEnvFromClosure _ _ env = env

-- ####################################################################################################


-- ####################################################################################################
-- Functions used to convert read Value to a value that can be stored.
-- ####################################################################################################

-- convertExpressionToValue takes an expression (read from input),
-- and converts to a Literal Value (using values stored in SAS and Env)
-- and doing computation
convertExpressionToValue :: Types.Expression -> Types.EnvironmentMap -> Types.SingleAssignmentStore -> Types.Value
convertExpressionToValue (Types.Lit x) env sas = Types.Liter x

convertExpressionToValue (Types.Variable x) env sas = if (isLit val)
                                                        then val
                                                        else error $ "Variable: " ++ x ++ " should be bound to a literal value in the expression."
  where val = getValue x env sas

convertExpressionToValue (Types.Exp op left right) env sas
  | op == Types.Add  = Types.Liter $ leftValue + rightValue
  | op == Types.Sub  = Types.Liter $ leftValue - rightValue
  | op == Types.Mult = Types.Liter $ leftValue * rightValue
  | op == Types.Div  = Types.Liter $ leftValue `div` rightValue
  where leftValue  = Types.litVal $ convertExpressionToValue left env sas
        rightValue = Types.litVal $ convertExpressionToValue right env sas

-- convertValuesReadToValue reads an input expression or Value and converts it,
-- to an appropriate Literal / Record or Closure to be stored in SAS.
convertValuesReadToValue :: Types.ValuesRead -> Types.EnvironmentMap -> Types.SingleAssignmentStore -> Types.Value
convertValuesReadToValue (Types.Record l m) env _ = Types.Rec l valueMap
  where valueMap = Map.map getBindingVar m
          where getBindingVar x = Maybe.fromJust (Map.lookup x env)

convertValuesReadToValue (Types.Proc p s) env _ = Types.Closure p s closureEnv
  where closureEnv = Map.restrictKeys env freeVariables
          where freeVariables = Set.difference variablesInProc (Set.fromList p)
                  where variablesInProc = getVariablesInStatement s

convertValuesReadToValue (Types.Expr exp) env sas = convertExpressionToValue exp env sas

-- ####################################################################################################


-- ####################################################################################################
-- Functions used get available variables in the statements / values (to get list of free variables)
-- ####################################################################################################

-- getVariablesInExpression gets an expression and gives set of available variables
getVariablesInExpression :: Types.Expression -> Set.Set Types.Identifier
getVariablesInExpression (Types.Lit _) = Set.empty
getVariablesInExpression (Types.Variable x) = Set.singleton x
getVariablesInExpression (Types.Exp _ left right) = Set.union expInLeft expInRight
  where expInLeft  = getVariablesInExpression left
        expInRight = getVariablesInExpression right

-- getVariablesInExpression gets an statement and gives set of available variables (recursively)
getVariablesInStatement :: Types.Statement -> Set.Set Types.Identifier
getVariablesInStatement Types.Skip = Set.empty
getVariablesInStatement (Types.Multiple x) = foldr Set.union Set.empty (map getVariablesInStatement x)
getVariablesInStatement (Types.Var x s) = Set.difference (getVariablesInStatement s) (Set.fromList [x])
getVariablesInStatement (Types.BindIdent x y) = Set.fromList [x, y]
getVariablesInStatement (Types.BindValue x v) = Set.union (Set.fromList [x]) (getVariablesInValuesRead v)
getVariablesInStatement _ = Set.empty

-- getVariablesInValuesRead gets a read Value and gives set of available variables
getVariablesInValuesRead :: Types.ValuesRead -> Set.Set Types.Identifier
getVariablesInValuesRead (Types.Record _ m) = Set.fromList (Map.elems m)
getVariablesInValuesRead (Types.Proc p s) = Set.difference (getVariablesInStatement s) (Set.fromList p)
getVariablesInValuesRead (Types.Expr exp) = getVariablesInExpression exp

-- ####################################################################################################


-- ####################################################################################################
-- Extra Functions
-- ####################################################################################################

updateOldEquivalenceClass :: Types.Memory -> Types.Memory -> Types.MemoryToEqClassMap -> Types.MemoryToEqClassMap
updateOldEquivalenceClass oldValue newValue eqMap = Map.map (\x -> if x == oldValue then newValue else x) eqMap

-- ####################################################################################################
