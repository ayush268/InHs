module Main where

import ExecuteProgram
import qualified Types

main :: IO ()
main = do
  input <- getLine
  let statement = read input :: Types.Statement
  let (sas, triggerStore, listOfEnvLists, listOfSas, listOfTriggerStores) = executeProgram statement
  putStrLn "###########################################################"
  print statement
  putStrLn "###########################################################"
  print listOfEnvLists
  putStrLn "###########################################################"
  print listOfTriggerStores
  putStrLn "###########################################################"
  print listOfSas
  putStrLn "###########################################################"
  print triggerStore
  putStrLn "###########################################################"
  print sas
  putStrLn "###########################################################"
