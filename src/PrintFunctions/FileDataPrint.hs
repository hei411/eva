module PrintFunctions.FileDataPrint where

import Datatype
import PrintFunctions.BTypePrint
import PrintFunctions.CExpPrint

fileDataPrint :: CompiledFilesData -> String
fileDataPrint fileData = case fileData of
  [] -> "-------------------------------------------------------------------------------------"
  (fileName, functions, typenames) : tl ->
    do
      let rest = fileDataPrint tl
      let separator = "-------------------------------------------------------------------------------------\n"
      let functionString =
            ( case functions of
                [] -> "No functions defined.\n"
                _ -> "FUNCTIONS\n" ++ functionsPrint functions
            )
      let typenameString =
            ( case typenames of
                [] -> "No type synonyms defined.\n\n"
                _ -> "TYPE SYNONYMS\n" ++ typenamesPrint typenames ++ "\n\n"
            )
      rest ++ "File: " ++ fileName ++ "\n\n" ++ typenameString ++ functionString ++ separator

functionsPrint :: TypeCheckedProgram -> String
functionsPrint functions = case functions of
  [] -> ""
  (name, cExp, bType, tp) : tl -> do
    let bTypeString = printBType 0 bType
    let rest = functionsPrint tl
    let tpString = case tp of
          [] -> ""
          _ -> "[" ++ printtps 0 tp ++ "]"
    rest ++ name ++ tpString ++ " : " ++ bTypeString ++ "\n" -- ++ printCExp 0 cExp ++ "\n"
  where
    printtps :: Integer -> [TypeProperty] -> String
    printtps n tps = case tps of
      [] -> ""
      tp : tps' ->
        ( case tp of
            Limit -> "Limit "
            Stable -> "Stable "
            Both -> "Limit Stable "
            None -> ""
        )
          ++ "p"
          ++ show (n)
          ++ ( case tps' of
                 [] -> ""
                 tp' : tps2 -> ", "
             )
          ++ printtps (n + 1) tps'

typenamesPrint :: TypenameList -> String
typenamesPrint functions = case functions of
  [] -> ""
  (name, bType, n) : tl -> do
    let bTypeString = printBType 0 bType
    let rest = typenamesPrint tl
    let varString = if n == 0 then " " else "(" ++ printVar 0 n ++ ")"
    rest ++ name ++ varString ++ " = " ++ bTypeString ++ "\n"
  where
    printVar :: Integer -> Integer -> String
    printVar n remaining = if remaining == 1 then "p" ++ show (n) else "p" ++ show (n) ++ ", " ++ printVar (n + 1) (remaining -1)
