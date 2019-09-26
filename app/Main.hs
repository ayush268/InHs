module Main where

import Execution
import qualified Types

main :: IO ()
main = do
  input <- getLine
  let statement = read input :: Types.Statement
  let (sas, memoryLeft, envList) = executeProgram statement
  print statement
  print envList
  print sas
