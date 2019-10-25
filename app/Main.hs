module Main where

import ExecuteProgram
import qualified Types

main :: IO ()
main = do
  input <- getLine
  let statement = read input :: Types.Statement
  let (sas, listOfEnvLists) = executeProgram statement
  print statement
  print listOfEnvLists
  print sas
