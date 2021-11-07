module MainFunctions.MainFunctions where

import Datatype

getMain :: CompiledFilesData -> (CExp, BType)
getMain compiledFilesData =
  do
    let (_, entryFileData, _) : _ = compiledFilesData
    getMainHelper entryFileData
  where
    getMainHelper :: TypeCheckedProgram -> (CExp, BType)
    getMainHelper l = case l of
      [] -> error "Can't find main program in entry file"
      (name, cExp, bType, tps) : x1 ->
        if "main" /= name
          then getMainHelper x1
          else if length tps /= 0 then error "main program can't be polymorphic!" else (cExp, bType)

getInterpreter :: [String] -> InterpreterMode
getInterpreter args = case args of
  [] -> Normal
  s : ss -> do
    let head = take 14 s
    if head == "--interpreter="
      then do
        let tl = drop 14 s
        case tl of
          "Normal" -> Normal
          "Safe" -> Safe
          "Lively" -> Lively
          "Fair" -> Fair
          "ISafe" -> ISafe
          "ILively" -> ILively
          "IFair" -> IFair
          _ -> error "Cant detect interpreter type"
      else getInterpreter ss

getStepNum :: [String] -> Integer
getStepNum args = case args of
  [] -> 10
  s : ss -> do
    let head = take 10 s
    if head == "--stepNum="
      then do
        let tl = drop 10 s
        read tl :: Integer
      else getStepNum ss
