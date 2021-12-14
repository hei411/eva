module Interpreter.ILivelyInterpreter where

import Datatype
import Interpreter.EvaluationInterpreter
import Interpreter.InputFunctions
import Interpreter.StoreFunctions
import PrintFunctions.CExpPrint

iLivelyInterpreter :: CExp -> BType -> Bool -> IO ()
iLivelyInterpreter cExp expectedBType isPeano =
  do
    putStrLn "Running ILively interpreter (For stream types to Until types):"
    let (s, l0) = addStoreElem (TicklessStore []) CExpUnit
    let exp = CExpApplication (CExpUnbox cExp) (CExpAdv l0)
    iLivelyInterpreterHelper exp s l0 1 expectedBType isPeano

iLivelyInterpreterHelper :: CExp -> Store -> CExp -> Integer -> BType -> Bool -> IO ()
iLivelyInterpreterHelper cExp s location nowNum expectedBType isPeano =
  do
    putStrLn ("Input expression for timestep " ++ show nowNum ++ ": ")
    (input) <- parseInputExp expectedBType isPeano
    let (maybeCExp', s', l', output) = iUntilStep cExp s location input
    case maybeCExp' of
      Nothing -> do
        putStrLn ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
        putStrLn "Halt!"
      Just ce -> do
        putStrLn ("Timestep " ++ show nowNum ++ ": " ++ printCExp 0 output)
        iLivelyInterpreterHelper ce s' l' (nowNum + 1) expectedBType isPeano

iUntilStep :: CExp -> Store -> CExp -> CExp -> (Maybe CExp, Store, CExp, CExp)
iUntilStep cExp s l input = do
  let (s', l') = transformInputStore s input l
  let (cExp', s'') = evaluationInterpreter cExp s'
  case s'' of
    TickStore x0 x1 ->
      case cExp' of
        CExpWait hd tl -> (Just (CExpAdv tl), TicklessStore x1, l', hd)
        CExpNow v -> (Nothing, TicklessStore x1, l', v)
        _ -> error "until Step semantics doesnt step into another until constructor"
    _ -> error "until step semantics doesnt produce a tickstore"