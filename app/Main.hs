module Main where

import Parser.MainParser
import ProgramAnalyzer.ProgramAnalyzer (mainProgramAnalyzer)
import StringFunctions.CommentHandler
import System.Environment (getArgs)

main :: IO ()
main = do
  -- Get file name from arguments
  (file_name : _) <- getArgs

  -- Start of real program
  -- Read from file
  file_content <- readFile file_name
  -- Remove comments
  let clean_file_content = commentRemover file_content
  -- Create parse tree
  let parse_tree = mainParser clean_file_content
  case parse_tree of
    -- Error in parsing
    Left parseError -> putStrLn (show (parseError))
    Right program -> do
      -- Parsing succeeded
      putStrLn ("Program is parsed correctly")
      (compiledFilesData) <- mainProgramAnalyzer file_name program
      putStrLn ("Program is type-checked correctly.")

--putStrLn (show (typeCheckedProgram))