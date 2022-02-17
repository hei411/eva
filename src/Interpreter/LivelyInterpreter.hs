module Interpreter.LivelyInterpreter where

import Datatype
import Interpreter.EvaluationInterpreter
import PrintFunctions.BTypePrint
import PrintFunctions.CExpPrint
import System.CPUTime
import Text.Printf

livelyInterpreter :: CExp -> Integer -> Bool -> IO ()
livelyInterpreter cExp stepNum isTime =
  do
    putStrLn "Running lively interpreter (For until types):"
    -- checkBTypeLively bType
    livelyInterpreterHelper (CExpUnbox cExp) (TicklessStore []) stepNum 1 isTime

livelyInterpreterHelper :: CExp -> Store -> Integer -> Integer -> Bool -> IO ()
livelyInterpreterHelper cExp s stepNum nowNum isTime =
  do
    start <- getCPUTime
    let (maybecExp', s', output) = untilStep cExp s
    case maybecExp' of
      Nothing -> do
        putStr ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
        end <- getCPUTime
        let diff = (fromIntegral (end - start)) / (10 ^ 12)
        if isTime
          then printf "    (%0.3f sec)\n" (diff :: Double)
          else printf "\n"
        putStrLn "Halt!"
      Just ce -> do
        putStr ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
        end <- getCPUTime
        let diff = (fromIntegral (end - start)) / (10 ^ 12)
        if isTime
          then printf "    (%0.3f sec)\n" (diff :: Double)
          else printf "\n"
        if mod nowNum stepNum == 0
          then do
            getChar
            livelyInterpreterHelper ce s' stepNum (nowNum + 1) isTime
          else livelyInterpreterHelper ce s' stepNum (nowNum + 1) isTime

untilStep :: CExp -> Store -> (Maybe CExp, Store, CExp)
untilStep cExp s =
  case s of
    TicklessStore elemList -> do
      let (cExp', s') = evaluationInterpreter cExp (TickStore elemList [])

      case s' of
        TickStore x0 x1 ->
          case cExp' of
            CExpWait hd tl -> (Just (CExpAdv tl), TicklessStore x1, hd)
            CExpNow v -> (Nothing, TicklessStore x1, v)
            _ -> error "until Step semantics doesnt step into another until constructor"
        _ -> error "until step semantics doesnt produce a tickstore"
    _ -> error "Should not happen. Non tickless store passed to until step"
