module Interpreter.LivelyInterpreter where

import Datatype
import Interpreter.EvaluationInterpreter
import PrintFunctions.BTypePrint
import PrintFunctions.CExpPrint

livelyInterpreter :: CExp -> Integer -> IO ()
livelyInterpreter cExp stepNum =
  do
    putStrLn "Running lively interpreter (For until types):"
    -- checkBTypeLively bType
    livelyInterpreterHelper (CExpUnbox cExp) (TicklessStore []) stepNum 1

livelyInterpreterHelper :: CExp -> Store -> Integer -> Integer -> IO ()
livelyInterpreterHelper cExp s stepNum nowNum =
  do
    let (maybecExp', s', output) = untilStep cExp s
    case maybecExp' of
      Nothing -> do
        putStrLn ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
        putStrLn "Halt!"
      Just ce -> do
        putStrLn ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
        if mod nowNum stepNum == 0
          then do
            getChar
            livelyInterpreterHelper ce s' stepNum (nowNum + 1)
          else livelyInterpreterHelper ce s' stepNum (nowNum + 1)

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
