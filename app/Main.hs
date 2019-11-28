module Main where

import ExecuteProgram
import qualified Types

main :: IO ()
main = do
  input <- getLine
  let statement = read input :: Types.Statement
  let (sas, triggerStore, mutableStore, listOfEnvLists, listOfSas, listOfTriggerStores, listOfMutableStores) = executeProgram statement
  putStrLn "###########################################################"
  print statement
  putStrLn "###########################################################"
  print sas
  putStrLn "###########################################################"
  print triggerStore
  putStrLn "###########################################################"
  print mutableStore
  putStrLn "###########################################################"
  print listOfEnvLists
  putStrLn "###########################################################"
  print listOfSas
  putStrLn "###########################################################"
  print listOfTriggerStores
  putStrLn "###########################################################"
  print listOfMutableStores
  putStrLn "###########################################################"
