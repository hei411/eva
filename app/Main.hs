module Main where

import Parser.MainParser
import ProgramAnalyzer.ProgramAnalyzer (mainProgramAnalyzer)
import StringFunctions.CommentHandler
import System.Environment (getArgs)
import TypeChecker.MainTypeChecker (mainTypeChecker)
import TypeChecker.ValidChecker

main :: IO ()
main = do
  (file_name : _) <- getArgs
  file_content <- readFile file_name
  let parse_tree = mainParser (commentRemover file_content)
  case parse_tree of
    Left parseError -> putStrLn (show (parseError))
    Right program -> do
      putStrLn (show (program))
      putStrLn ("Program is parsed correctly")
      typeCheckedProgram <- mainProgramAnalyzer program
      putStrLn ("Program is type-checked correctly.")

--putStrLn (show (typeCheckedProgram))