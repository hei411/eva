module Interpreter.ISafeInterpreter where

import Datatype
import GHC.IO (evaluate)
import Interpreter.EvaluationInterpreter (evaluationInterpreter)
import Interpreter.InputFunctions
import Interpreter.StoreFunctions
import PrintFunctions.CExpPrint

iSafeInterpreter :: CExp -> Integer -> BType -> IO ()
iSafeInterpreter cExp stepNum expectedBType =
  do
    putStrLn "Running ISafe interpreter (For stream types to stream types):"
    let (s, l0) = addStoreElem (TicklessStore []) CExpUnit
    let exp = CExpApplication (CExpUnbox cExp) (CExpAdv l0)
    iSafeInterpreterHelper exp s l0 stepNum 1 expectedBType

iSafeInterpreterHelper :: CExp -> Store -> CExp -> Integer -> Integer -> BType -> IO ()
iSafeInterpreterHelper cExp s location stepNum nowNum expectedBType =
  do
    putStrLn ("Input expression for timestep " ++ show nowNum ++ ": ")
    (input) <- parseInputExp expectedBType
    let (cExp', s', l', output) = iStreamStep cExp s location input
    putStrLn ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
    if mod nowNum stepNum == 0
      then do
        getChar
        iSafeInterpreterHelper cExp' s' l' stepNum (nowNum + 1) expectedBType
      else iSafeInterpreterHelper cExp' s' l' stepNum (nowNum + 1) expectedBType

iStreamStep :: CExp -> Store -> CExp -> CExp -> (CExp, Store, CExp, CExp)
iStreamStep cExp s l input = do
  let TicklessStore lis = s
  let (s', l') = addStoreElem (TickStore lis []) CExpUnit
  let TickStore a b = s'
  let CExpLocation n = l
  let a' = modifyStoreElem a n (CExpInto (CExpProduct input l'))
  let newStore = TickStore a' b
  error "Not yet implementedd"