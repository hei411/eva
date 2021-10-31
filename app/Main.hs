module Main where

import ProgramAnalyzer.ProgramAnalyzer (mainProgramAnalyzer)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- Get file name from arguments
  (file_name : _) <- getArgs
  let src_path = ""
  -- Start of real program
  compiledFilesData <- mainProgramAnalyzer src_path file_name
  return ()