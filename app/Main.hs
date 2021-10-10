module Main where

import Parser.MainParser
import System.Environment (getArgs)
import TypeChecker.ValidChecker

main :: IO ()
main = do
  (file_name : _) <- getArgs
  file_content <- readFile file_name
  let parse_tree = mainParser file_content
  case parse_tree of
    Left parseError -> fail (show (parseError))
    Right program -> isValidProgramMain program
