module Interpreter.FairInterpreter where

import Datatype
import Interpreter.LivelyInterpreter
import PrintFunctions.CExpPrint

fairInterpreter :: CExp -> Integer -> IO ()
fairInterpreter cExp stepNum =
  do
    putStrLn "Running fair interpreter (For fair types):"
    -- checkBTypeLively bType
    fairInterpreterHelper (CExpOut (CExpUnbox cExp)) (TicklessStore []) 1 stepNum 1

fairInterpreterHelper :: CExp -> Store -> Integer -> Integer -> Integer -> IO ()
fairInterpreterHelper cExp s mode stepNum nowNum =
  do
    let (cExp', s', mode', output) = fairStep cExp s mode
    putStrLn ("Timestep " ++ show nowNum ++ " (Mode " ++ show mode' ++ "): " ++ printCExp 0 output)
    if mod nowNum stepNum == 0
      then do
        getChar
        fairInterpreterHelper cExp' s' mode' stepNum (nowNum + 1)
      else fairInterpreterHelper cExp' s' mode' stepNum (nowNum + 1)

fairStep :: CExp -> Store -> Integer -> (CExp, Store, Integer, CExp)
fairStep cExp s mode =
  do
    let (maybeCExp', s', output) = untilStep cExp s
    case maybeCExp' of
      Nothing -> do
        case output of
          CExpProduct v w ->
            if mode == 1
              then (CExpAdv w, s', 2, v)
              else (CExpOut (CExpAdv w), s', 1, v)
          _ -> error "fair Step semantics doesnt step into a product when the until step halts"
      Just ce -> (ce, s', mode, output)
