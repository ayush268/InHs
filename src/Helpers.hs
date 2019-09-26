-- Module for helper functions

module Helpers
  (isRec,
   convertValuesReadToValue) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import qualified Types

isRec :: Types.Value -> Bool
isRec (Types.Rec x y) = True
isRec _ = False

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
