module Interpreter.FairInterpreter where

import Datatype
import Interpreter.LivelyInterpreter
import PrintFunctions.CExpPrint
import System.Clock
import Text.Printf

fairInterpreter :: CExp -> Integer -> Bool -> IO ()
fairInterpreter cExp stepNum isTime =
  do
    putStrLn "Running fair interpreter (For fair types):"
    -- checkBTypeLively bType
    fairInterpreterHelper (CExpOut (CExpUnbox cExp)) (TicklessStore []) 1 stepNum 1 isTime

fairInterpreterHelper :: CExp -> Store -> Integer -> Integer -> Integer -> Bool -> IO ()
fairInterpreterHelper cExp s mode stepNum nowNum isTime =
  do
    start <- getTime Monotonic
    let (cExp', s', mode', output) = fairStep cExp s mode
    putStr ("Timestep " ++ show nowNum ++ " (Mode " ++ show mode' ++ "): " ++ printCExp 0 output)
    end <- getTime Monotonic
    let diff = fromIntegral (toNanoSecs (diffTimeSpec end start)) / (10 ^ 9)
    if isTime
      then printf "    (%0.3f sec)\n" (diff :: Double)
      else printf "\n"
    if mod nowNum stepNum == 0
      then do
        getChar
        fairInterpreterHelper cExp' s' mode' stepNum (nowNum + 1) isTime
      else fairInterpreterHelper cExp' s' mode' stepNum (nowNum + 1) isTime

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
