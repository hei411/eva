module Interpreter.ISafeInterpreter where

import Datatype
import GHC.IO (evaluate)
import Interpreter.EvaluationInterpreter (evaluationInterpreter)
import Interpreter.InputFunctions
import Interpreter.StoreFunctions
import PrintFunctions.CExpPrint (printCExp)
import System.CPUTime
import Text.Printf

iSafeInterpreter :: CExp -> BType -> Bool -> Bool -> IO ()
iSafeInterpreter cExp expectedBType isPeano isTime =
  do
    putStrLn "Running ISafe interpreter (For stream types to stream types):"
    let (s, l0) = addStoreElem (TicklessStore []) CExpUnit
    let exp = CExpApplication (CExpUnbox cExp) (CExpAdv l0)
    iSafeInterpreterHelper exp s l0 1 expectedBType isPeano isTime

iSafeInterpreterHelper :: CExp -> Store -> CExp -> Integer -> BType -> Bool -> Bool -> IO ()
iSafeInterpreterHelper cExp s location nowNum expectedBType isPeano isTime =
  do
    putStrLn ("Input expression for timestep " ++ show nowNum ++ ": ")
    (input) <- parseInputExp expectedBType isPeano
    start <- getCPUTime
    let (cExp', s', l', output) = iStreamStep cExp s location input
    putStr ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10 ^ 12)
    if isTime
      then printf "    (%0.3f sec)\n" (diff :: Double)
      else printf "\n"
    iSafeInterpreterHelper cExp' s' l' (nowNum + 1) expectedBType isPeano isTime

iStreamStep :: CExp -> Store -> CExp -> CExp -> (CExp, Store, CExp, CExp)
iStreamStep cExp s l input = do
  let (s', l') = transformInputStore s input l
  let (cExp', s'') = evaluationInterpreter cExp s'
  case cExp' of
    CExpInto (CExpProduct hd tl) -> case s'' of
      TickStore x0 x1 -> (CExpAdv tl, TicklessStore x1, l', hd)
      _ -> error "istream step semantics doesnt produce a tickstore"
    _ -> error "istream Step semantics doesnt step into another stream"