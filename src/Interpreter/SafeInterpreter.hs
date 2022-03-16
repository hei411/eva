module Interpreter.SafeInterpreter where

import Datatype
import Interpreter.EvaluationInterpreter
import PrintFunctions.CExpPrint (printCExp)
import System.Clock
import Text.Printf

safeInterpreter :: CExp -> Integer -> Bool -> IO ()
safeInterpreter cExp stepNum isTime =
  do
    putStrLn "Running safe interpreter (For stream types):"
    --checkBTypeSafe bType
    safeInterpreterHelper (CExpUnbox cExp) (TicklessStore []) stepNum 1 isTime

safeInterpreterHelper :: CExp -> Store -> Integer -> Integer -> Bool -> IO ()
safeInterpreterHelper cExp s stepNum nowNum isTime =
  do
    start <- getTime Monotonic
    let (cExp', s', output) = streamStep cExp s
    end <- cExp' `seq` getTime Monotonic
    putStr ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
    let diff = fromIntegral (toNanoSecs (diffTimeSpec end start)) / (10 ^ 9)
    if isTime
      then printf "    (%0.3f sec)\n" (diff :: Double)
      else printf "\n"
    if mod nowNum stepNum == 0
      then do
        getChar
        safeInterpreterHelper cExp' s' stepNum (nowNum + 1) isTime
      else safeInterpreterHelper cExp' s' stepNum (nowNum + 1) isTime

streamStep :: CExp -> Store -> (CExp, Store, CExp)
streamStep cExp s =
  case s of
    TicklessStore elemList -> do
      let (cExp', s') = evaluationInterpreter cExp (TickStore elemList [])
      case cExp' of
        CExpInto (CExpProduct hd tl) -> case s' of
          TickStore x0 x1 -> (CExpAdv tl, TicklessStore x1, hd)
          _ -> error "stream step semantics doesnt produce a tickstore"
        _ -> error "stream Step semantics doesnt step into another stream"
    _ -> error "Should not happen! non tickless store passed to streamStep"
