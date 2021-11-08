module MainFunctions.MainFunctions where

import Datatype
import TypeFunctions.TypeCompare

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

getInterpreter :: BType -> InterpreterMode
getInterpreter bType =
  case bType of
    BTypeBox (BTypeFix (BTypeProduct _ (BTypeIndex 0))) -> Safe
    BTypeBox (BTypeUntil _ _) -> Lively
    BTypeBox (BTypeFix (BTypeUntil a (BTypeProduct b (BTypeAngle (BTypeUntil b' (BTypeProduct a' (BTypeIndex 0))))))) ->
      if generalBTypeCompare a a' && generalBTypeCompare b b' then Fair else Normal
    _ -> Normal

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

getSrcPath :: [String] -> FilePath
getSrcPath args = case args of
  [] -> ""
  s : ss -> do
    let head = take 6 s
    if head == "--src="
      then do
        let tl = drop 6 s
        if last tl /= '/' then tl ++ "/" else tl
      else getSrcPath ss
