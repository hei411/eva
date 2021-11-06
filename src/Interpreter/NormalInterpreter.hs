module Interpreter.NormalInterpreter where

import Datatype
import Interpreter.EvaluationInterpreter (evaluationInterpreter)
import PrintFunctions.CExpPrint (printCExp)

normalInterpreter :: CExp -> IO ()
normalInterpreter cExp =
  do
    putStrLn "Running normal interpreter:"
    let (cExp', _) = evaluationInterpreter cExp NullStore
    putStrLn (printCExp 0 cExp')
    putStrLn "Halt!\n"