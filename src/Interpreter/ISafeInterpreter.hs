module Interpreter.ISafeInterpreter where

import Datatype
import GHC.IO (evaluate)
import Interpreter.EvaluationInterpreter (evaluationInterpreter)
import Interpreter.InputFunctions
import Interpreter.StoreFunctions
import PrintFunctions.CExpPrint

iSafeInterpreter :: CExp -> BType -> IO ()
iSafeInterpreter cExp expectedBType =
  do
    putStrLn "Running ISafe interpreter (For stream types to stream types):"
    let (s, l0) = addStoreElem (TicklessStore []) CExpUnit
    let exp = CExpApplication (CExpUnbox cExp) (CExpAdv l0)
    iSafeInterpreterHelper exp s l0 1 expectedBType

iSafeInterpreterHelper :: CExp -> Store -> CExp -> Integer -> BType -> IO ()
iSafeInterpreterHelper cExp s location nowNum expectedBType =
  do
    putStrLn ("Input expression for timestep " ++ show nowNum ++ ": ")
    (input) <- parseInputExp expectedBType
    let (cExp', s', l', output) = iStreamStep cExp s location input
    putStrLn ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
    iSafeInterpreterHelper cExp' s' l' (nowNum + 1) expectedBType

iStreamStep :: CExp -> Store -> CExp -> CExp -> (CExp, Store, CExp, CExp)
iStreamStep cExp s l input = do
  let (s', l') = transformInputStore s input l
  let (cExp', s'') = evaluationInterpreter cExp s'
  case cExp' of
    CExpInto (CExpProduct hd tl) -> case s'' of
      TickStore x0 x1 -> (CExpAdv tl, TicklessStore x1, l', hd)
      _ -> error "istream step semantics doesnt produce a tickstore"
    _ -> error "istream Step semantics doesnt step into another stream"