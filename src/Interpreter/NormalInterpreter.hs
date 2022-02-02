module Interpreter.NormalInterpreter where

import Datatype
import Interpreter.EvaluationInterpreter (evaluationInterpreter)
import PrintFunctions.CExpPrint (printCExp)
import System.CPUTime
import Text.Printf

normalInterpreter :: CExp -> Bool -> IO ()
normalInterpreter cExp isTime =
  do
    putStrLn "Running normal interpreter:"
    start <- getCPUTime
    let (cExp', _) = evaluationInterpreter cExp NullStore
    putStr (printCExp 0 cExp')
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10 ^ 12)
    if isTime
      then printf "    (%0.3f sec)\n" (diff :: Double)
      else printf "\n"
    putStrLn "Halt!\n"