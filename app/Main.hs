module Main where

import ExecuteProgram
import qualified Types

main :: IO ()
main = do
  input <- getLine
  let statement = read input :: Types.Statement
  let (sas, listOfEnvLists, listOfSas) = executeProgram statement
  putStrLn "###########################################################"
  print statement
  putStrLn "###########################################################"
  print listOfEnvLists
  putStrLn "###########################################################"
  print listOfSas
  putStrLn "###########################################################"
  print sas
  putStrLn "###########################################################"
