module Main where

import PrintFunctions.FileDataPrint
import ProgramAnalyzer.ProgramAnalyzer (mainProgramAnalyzer)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- Get file name from arguments
  (file_name : _) <- getArgs
  -- get flag for src_path
  let src_path = ""
  -- Start of real program
  compiledFilesData <- mainProgramAnalyzer src_path file_name
  putStrLn (fileDataPrint (compiledFilesData))
  return ()
