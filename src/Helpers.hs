-- Module for helper functions

module Helpers
  (isLit,
   isRec,
   isClosure,
   isRecord,
   getValue,
   matchPattern,
   extendEnvFromPattern,
   extendEnvFromClosure,
   convertValuesReadToValue) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import qualified Types

isLit :: Types.Value -> Bool
isLit (Types.Liter _) = True
isLit _ = False

isRec :: Types.Value -> Bool
isRec (Types.Rec _ _) = True
isRec _ = False

isClosure :: Types.Value -> Bool
isClosure (Types.Closure _ _ _) = True
isClosure _ = False

isRecord :: Types.ValuesRead -> Bool
isRecord (Types.Record _ _) = True
isRecord _ = False

getValue :: Types.Identifier -> Types.EnvironmentMap -> Types.SingleAssignmentStore -> Types.Value
getValue src env (eqMap, valueMap) = if (Maybe.isJust var) && (Maybe.isJust eqClass) && (Maybe.isJust val)
                                       then (Maybe.fromJust val)
                                       else error $ "Variable " ++ src ++ " either not in scope or not bound to a value."
  where var = Map.lookup src env
        eqClass = Map.lookup (Maybe.fromJust var) eqMap
        val = Map.lookup (Maybe.fromJust eqClass) valueMap

matchPattern :: Types.Value -> Types.ValuesRead -> Bool
matchPattern (Types.Rec a b) (Types.Record c d) = labelMatch && arityMatch && featuresMatch
  where labelMatch = (a == c)
        arityMatch = ((length b) == (length d))
        featuresMatch = ((length $ Set.union keysB keysD) == (length $ Set.intersection keysB keysD))
          where keysB = Set.fromList $ Map.keys b
                keysD = Set.fromList $ Map.keys d

matchPattern _ _ = False


extendEnvFromPattern :: Types.Value -> Types.ValuesRead -> Types.EnvironmentMap -> Types.EnvironmentMap
extendEnvFromPattern (Types.Rec a val) (Types.Record b pattern) env = Map.union patternEnvMap env  -- Map union is left biased
  where patternEnvMap = Map.fromList $ zip patternIdents valMemories
          where patternIdents = Map.elems pattern
                valMemories = Map.elems val

extendEnvFromPattern _ _ env = env


extendEnvFromClosure :: Types.Value -> [Types.Identifier] -> Types.EnvironmentMap -> Types.EnvironmentMap
extendEnvFromClosure (Types.Closure closureParams _ closureEnv) params env = Map.union closureEnvMap closureEnv -- Map union is left biased
  where closureEnvMap = Map.fromList $ zip closureParams memoryOfParams
          where memoryOfParams = map (\x -> Maybe.fromJust $ Map.lookup x env) params

extendEnvFromClosure _ _ env = env


convertValuesReadToValue :: Types.ValuesRead -> Types.EnvironmentMap -> Types.Value
convertValuesReadToValue (Types.Lit x) _ = Types.Liter x

convertValuesReadToValue (Types.Record l m) env = Types.Rec l valueMap
  where valueMap = Map.map getBindingVar m
          where getBindingVar x = Maybe.fromJust (Map.lookup x env)

convertValuesReadToValue (Types.Proc p s) env = Types.Closure p s closureEnv
  where closureEnv = Map.restrictKeys env freeVariables
          where freeVariables = Set.difference variablesInProc (Set.fromList p)
                  where variablesInProc = getVariablesInStatement s

getVariablesInStatement :: Types.Statement -> Set.Set Types.Identifier

getVariablesInStatement Types.Skip = Set.empty
getVariablesInStatement (Types.Multiple x) = foldr Set.union Set.empty (map getVariablesInStatement x)
getVariablesInStatement (Types.Var x s) = Set.difference (getVariablesInStatement s) (Set.fromList [x])
getVariablesInStatement (Types.BindIdent x y) = Set.fromList [x, y]
getVariablesInStatement (Types.BindValue x v) = Set.union (Set.fromList [x]) (getVariablesInValuesRead v)
getVariablesInStatement _ = Set.empty

getVariablesInValuesRead :: Types.ValuesRead -> Set.Set Types.Identifier
getVariablesInValuesRead (Types.Lit _) = Set.empty
getVariablesInValuesRead (Types.Record _ m) = Set.fromList (Map.elems m)
getVariablesInValuesRead (Types.Proc p s) = Set.difference (getVariablesInStatement s) (Set.fromList p)
