module Main where

import Datatype
import Interpreter.LivelyInterpreter
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
  let src_path = getSrcPath args
  compiledFilesData <- mainProgramAnalyzer src_path file_name
  putStr (fileDataPrint (compiledFilesData))
  let (mainExp, mainType) = getMain compiledFilesData
  let interpreterType = getInterpreter mainType
  let stepNum = getStepNum args
  case interpreterType of
    Normal -> normalInterpreter mainExp
    Safe -> safeInterpreter mainExp  stepNum
    Lively -> livelyInterpreter mainExp  stepNum
    _ -> error "interpreter type not implemented"

  return ()
