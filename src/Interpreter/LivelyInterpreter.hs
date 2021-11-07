module Interpreter.LivelyInterpreter where

import Datatype
import Interpreter.EvaluationInterpreter
import PrintFunctions.BTypePrint
import PrintFunctions.CExpPrint

livelyInterpreter :: CExp -> BType -> Integer -> IO ()
livelyInterpreter cExp bType stepNum =
  do
    putStrLn "Running lively interpreter (For until types):"
    checkBTypeLively bType
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
untilStep cExp s = do
  let TicklessStore elemList = s
  let (cExp', s') = evaluationInterpreter cExp (TickStore elemList [])

  case s' of
    TickStore x0 x1 ->
      case cExp' of
        CExpWait hd tl -> (Just (CExpAdv tl), TicklessStore x1, hd)
        CExpNow v -> (Nothing, TicklessStore x1, v)
        _ -> error "until Step semantics doesnt step into another until constructor"
    _ -> error "until step semantics doesnt produce a tickstore"

checkBTypeLively :: BType -> IO ()
checkBTypeLively bType = case bType of
  BTypeBox (BTypeUntil _ _) -> return ()
  _ -> error ("main function has type " ++ printBType 0 bType ++ " which is not a valid type for the lively interpreter")