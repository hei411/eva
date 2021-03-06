module Interpreter.OneStepInterpreter where

import Datatype
import Interpreter.EvaluationInterpreter (evaluationInterpreter)
import PrintFunctions.CExpPrint (printCExp)
import System.Clock
import Text.Printf

oneStepInterpreter :: CExp -> Bool -> IO ()
oneStepInterpreter cExp isTime =
  do
    putStrLn "Running oneStepp interpreter:"
    start <- getTime Monotonic
    let (cExp', _) = evaluationInterpreter cExp NullStore
    end <- cExp' `seq` getTime Monotonic
    putStr (printCExp 0 cExp')
    let diff = fromIntegral (toNanoSecs (diffTimeSpec end start)) / (10 ^ 9)
    if isTime
      then printf "    (%0.3f sec)\n" (diff :: Double)
      else printf "\n"
    putStrLn "Halt!\n"