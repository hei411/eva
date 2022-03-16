module Interpreter.IFairInterpreter where

import Datatype
import Interpreter.ILivelyInterpreter
import Interpreter.InputFunctions
import Interpreter.StoreFunctions
import PrintFunctions.CExpPrint
import System.Clock
import Text.Printf

iFairInterpreter :: CExp -> BType -> Bool -> Bool -> IO ()
iFairInterpreter cExp expectedBType isPeano isTime =
  do
    putStrLn "Running IFair interpreter (For stream types to fair types):"
    let (s, l0) = addStoreElem (TicklessStore []) CExpUnit
    let exp = CExpOut (CExpApplication (CExpUnbox cExp) (CExpAdv l0))
    iFairInterpreterHelper exp s l0 1 1 expectedBType isPeano isTime

iFairInterpreterHelper :: CExp -> Store -> CExp -> Integer -> Integer -> BType -> Bool -> Bool -> IO ()
iFairInterpreterHelper cExp s location mode nowNum expectedBType isPeano isTime = do
  putStrLn ("Input expression for timestep " ++ show nowNum ++ ": ")
  (input) <- parseInputExp expectedBType isPeano
  start <- getTime Monotonic
  let (cExp', s', l', mode', output) = iFairStep cExp s location mode input
  end <- getTime Monotonic
  putStr ("Timestep " ++ show nowNum ++ " (Mode " ++ show mode' ++ "): " ++ printCExp 0 output)
  let diff = fromIntegral (toNanoSecs (diffTimeSpec end start)) / (10 ^ 9)
  if isTime
    then printf "    (%0.3f sec)\n" (diff :: Double)
    else printf "\n"
  iFairInterpreterHelper cExp' s' l' mode' (nowNum + 1) expectedBType isPeano isTime

iFairStep :: CExp -> Store -> CExp -> Integer -> CExp -> (CExp, Store, CExp, Integer, CExp)
iFairStep cExp s location mode input =
  do
    let (maybeCExp', s', l', output) = iUntilStep cExp s location input
    case maybeCExp' of
      Nothing -> do
        case output of
          CExpProduct v w ->
            if mode == 1
              then (CExpAdv w, s', l', 2, v)
              else (CExpOut (CExpAdv w), s', l', 1, v)
          _ -> error "ifair Step semantics doesnt step into a product when the iuntil step halts"
      Just ce -> (ce, s', l', mode, output)