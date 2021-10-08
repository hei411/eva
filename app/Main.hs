module Main where

import Datatype
import Parser
import System.Environment (getArgs)

main :: IO ()
main = do
  (file_name : _) <- getArgs
  file_content <- readFile file_name
  let parse_tree = mainParser file_content
  putStrLn (show (parse_tree))
  putStrLn (show (AExpProduct (AExpSuc AExpZero) AExpZero))