-- Module for the Semantic Stack object and its methods

--module semantic_stack
--  (executeStack) where

import qualified Types
import qualified Data.Map as Map

executeStack :: [(Types.Statement, Types.EnvironmentMap)] -> Bool

executeStack = foldl foldingFunction True
  where foldingFunction result (Types.Skip, env) = True && result
        foldingFunction result ((Types.Multiple x), env) = (executeStack semanticStack) && result
          where semanticStack = map (\b -> (b, env)) x
        foldingFunction result _ = False

executeProgram :: Types.Statement -> Bool
executeProgram x = executeStack [(x, Map.empty)]
