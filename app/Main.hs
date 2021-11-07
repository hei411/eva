module Main where

import Datatype
import Interpreter.NormalInterpreter (normalInterpreter)
import Interpreter.SafeInterpreter (safeInterpreter)
import MainFunctions.MainFunctions
import PrintFunctions.FileDataPrint
import ProgramAnalyzer.ProgramAnalyzer (mainProgramAnalyzer)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- Get file name from arguments
  (file_name : args) <- getArgs
  -- get flag for src_path
  let src_path = ""
  -- Start of real program
  compiledFilesData <- mainProgramAnalyzer src_path file_name
  putStr (fileDataPrint (compiledFilesData))
  let (mainExp, mainType) = getMain compiledFilesData
  let interpreterType = getInterpreter args
  let stepNum = getStepNum args
  case interpreterType of
    Normal -> normalInterpreter mainExp
    Safe -> safeInterpreter mainExp mainType stepNum
    _ -> error "interpreter type not implemented"

  return ()
