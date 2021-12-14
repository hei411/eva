module Interpreter.IFairInterpreter where

import Datatype
import Interpreter.ILivelyInterpreter
import Interpreter.InputFunctions
import Interpreter.StoreFunctions
import PrintFunctions.CExpPrint

iFairInterpreter :: CExp -> BType -> Bool -> IO ()
iFairInterpreter cExp expectedBType isPeano =
  do
    putStrLn "Running IFair interpreter (For stream types to fair types):"
    let (s, l0) = addStoreElem (TicklessStore []) CExpUnit
    let exp = CExpOut (CExpApplication (CExpUnbox cExp) (CExpAdv l0))
    iFairInterpreterHelper exp s l0 1 1 expectedBType isPeano

iFairInterpreterHelper :: CExp -> Store -> CExp -> Integer -> Integer -> BType -> Bool -> IO ()
iFairInterpreterHelper cExp s location mode nowNum expectedBType isPeano = do
  putStrLn ("Input expression for timestep " ++ show nowNum ++ ": ")
  (input) <- parseInputExp expectedBType isPeano
  let (cExp', s', l', mode', output) = iFairStep cExp s location mode input
  putStrLn ("Timestep " ++ show nowNum ++ " (Mode " ++ show mode' ++ "): " ++ printCExp 0 output)
  iFairInterpreterHelper cExp' s' l' mode' (nowNum + 1) expectedBType isPeano

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