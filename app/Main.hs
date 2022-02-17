module Main where

import Datatype
import Foreign (finalizeForeignPtr)
import Interpreter.FairInterpreter
import Interpreter.IFairInterpreter
import Interpreter.ILivelyInterpreter (iLivelyInterpreter)
import Interpreter.ISafeInterpreter
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
  let isPeano = checkPeano args
  compiledFilesData <- mainProgramAnalyzer src_path isPeano file_name
  putStr (fileDataPrint (compiledFilesData))
  let mainMaybe = getMain compiledFilesData
  case mainMaybe of
    Nothing -> putStrLn "Can't find main program in entry file"
    Just (mainExp, mainType) ->
      do
        let interpreterType = getInterpreter mainType
        let stepNum = getStepNum args
        let isTime = checkTime args
        case interpreterType of
          Normal -> normalInterpreter mainExp isTime
          Safe -> safeInterpreter mainExp stepNum isTime
          Lively -> livelyInterpreter mainExp stepNum isTime
          Fair -> fairInterpreter mainExp stepNum isTime
          ISafe -> iSafeInterpreter mainExp (getInputType mainType) isPeano isTime
          ILively -> iLivelyInterpreter mainExp (getInputType mainType) isPeano isTime
          IFair -> iFairInterpreter mainExp (getInputType mainType) isPeano isTime

        return ()
