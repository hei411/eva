module Interpreter.InputFunctions where

import Datatype
import ExpTypeConverters.ABExpConverter
import Parser.ExpParser
import PrintFunctions.BTypePrint (printBType)
import Text.Parsec
import TypeChecker.MainTypeChecker
import TypeFunctions.TypeCompare

parseInputExp :: BType -> IO (CExp)
parseInputExp bType = do
  input <- getLine
  let parseResult = parse expParser "Error in parsing input" input
  case parseResult of
    Left pe -> do
      putStrLn ("Error in parsing input! Try again!")
      parseInputExp bType
    Right ae -> do
      let bExp = abExpConverter "Input terminal" "Input" [] [] ae
      let (cExp, checkedType) = (mainTypeChecker "Input terminal" "Input" [] (StableContext [] []) [] bExp)
      if generalBTypeCompare checkedType bType
        then return (cExp)
        else do
          putStrLn ("Type is " ++ printBType 0 checkedType ++ " but expecting " ++ printBType 0 bType)
          parseInputExp bType