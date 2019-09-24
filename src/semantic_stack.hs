-- Module for the Semantic Stack object and its methods

-- module semantic_stack(..) where

import qualified Types as T

executeStack :: [T.Statement] -> Bool

executeStack = foldl foldingFunction True
  where foldingFunction result T.Skip = True && result
        foldingFunction result (T.Multiple x) = (executeStack x) && result
        foldingFunction result _ = False
