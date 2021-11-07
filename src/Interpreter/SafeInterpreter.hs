module Interpreter.SafeInterpreter where

import Datatype
import Interpreter.EvaluationInterpreter
import PrintFunctions.BTypePrint
import PrintFunctions.CExpPrint (printCExp)

safeInterpreter :: CExp -> BType -> Integer -> IO ()
safeInterpreter cExp bType stepNum =
  do
    putStrLn "Running normal interpreter:"
    checkBTypeSafe bType
    safeInterpreterHelper (CExpUnbox cExp) (TicklessStore []) stepNum 1

safeInterpreterHelper :: CExp -> Store -> Integer -> Integer -> IO ()
safeInterpreterHelper cExp s stepNum nowNum =
  do
    let (cExp', s', output) = streamStep cExp s
    putStrLn ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
    if mod nowNum stepNum == 0
      then do
        getChar
        safeInterpreterHelper cExp' s' stepNum (nowNum + 1)
      else safeInterpreterHelper cExp' s' stepNum (nowNum + 1)

streamStep :: CExp -> Store -> (CExp, Store, CExp)
streamStep cExp s =
  do
    let TicklessStore elemList = s
    let (cExp', s') = evaluationInterpreter cExp (TickStore elemList [])
    case cExp' of
      CExpInto (CExpProduct hd tl) -> case s' of
        TickStore x0 x1 -> (CExpAdv tl, TicklessStore x1, hd)
        _ -> error "stream step semantics doesnt produce a tickstore"
      _ -> error "stream Step semantics doesnt step into another stream"

checkBTypeSafe :: BType -> IO ()
checkBTypeSafe bType = case bType of
  BTypeBox (BTypeFix (BTypeProduct _ (BTypeIndex 0))) -> return ()
  _ -> error ("main function has type " ++ printBType 0 bType ++ " which is not a valid type for the safe interpreter")