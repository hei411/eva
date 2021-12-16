module Interpreter.ISafeInterpreter where

import Datatype
import GHC.IO (evaluate)
import Interpreter.EvaluationInterpreter (evaluationInterpreter)
import Interpreter.InputFunctions
import Interpreter.StoreFunctions
import PrintFunctions.CExpPrint (printCExp)

iSafeInterpreter :: CExp -> BType -> Bool -> IO ()
iSafeInterpreter cExp expectedBType isPeano =
  do
    putStrLn "Running ISafe interpreter (For stream types to stream types):"
    let (s, l0) = addStoreElem (TicklessStore []) CExpUnit
    let exp = CExpApplication (CExpUnbox cExp) (CExpAdv l0)
    iSafeInterpreterHelper exp s l0 1 expectedBType isPeano

iSafeInterpreterHelper :: CExp -> Store -> CExp -> Integer -> BType -> Bool -> IO ()
iSafeInterpreterHelper cExp s location nowNum expectedBType isPeano =
  do
    putStrLn ("Input expression for timestep " ++ show nowNum ++ ": ")
    (input) <- parseInputExp expectedBType isPeano
    let (cExp', s', l', output) = iStreamStep cExp s location input
    putStrLn ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
    iSafeInterpreterHelper cExp' s' l' (nowNum + 1) expectedBType isPeano

iStreamStep :: CExp -> Store -> CExp -> CExp -> (CExp, Store, CExp, CExp)
iStreamStep cExp s l input = do
  let (s', l') = transformInputStore s input l
  let (cExp', s'') = evaluationInterpreter cExp s'
  case cExp' of
    CExpInto (CExpProduct hd tl) -> case s'' of
      TickStore x0 x1 -> (CExpAdv tl, TicklessStore x1, l', hd)
      _ -> error "istream step semantics doesnt produce a tickstore"
    _ -> error "istream Step semantics doesnt step into another stream"