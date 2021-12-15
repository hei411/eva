module MainFunctions.MainFunctions where

import Datatype
import TypeFunctions.TypeCompare

getMain :: CompiledFilesData -> Maybe (CExp, BType)
getMain compiledFilesData =
  do
    let (_, entryFileData, _) : _ = compiledFilesData
    getMainHelper entryFileData
  where
    getMainHelper :: TypeCheckedProgram -> Maybe (CExp, BType)
    getMainHelper l = case l of
      [] -> Nothing
      (name, cExp, bType, tps) : x1 ->
        if "main" /= name
          then getMainHelper x1
          else if length tps /= 0 then error "main program can't be polymorphic!" else Just (cExp, bType)

getInterpreter :: BType -> InterpreterMode
getInterpreter bType =
  case bType of
    BTypeBox (BTypeFix (BTypeProduct _ (BTypeIndex 0))) -> Safe
    BTypeBox (BTypeUntil _ _) -> Lively
    BTypeBox (BTypeFix (BTypeUntil a (BTypeProduct b (BTypeAngle (BTypeUntil b' (BTypeProduct a' (BTypeIndex 0))))))) ->
      if generalBTypeCompare a a' && generalBTypeCompare b b' then Fair else Normal
    BTypeBox (BTypeFunction (BTypeFix (BTypeProduct _ (BTypeIndex 0))) (BTypeFix (BTypeProduct _ (BTypeIndex 0)))) ->
      ISafe
    BTypeBox (BTypeFunction (BTypeFix (BTypeProduct _ (BTypeIndex 0))) (BTypeUntil _ _)) ->
      ILively
    BTypeBox
      ( BTypeFunction
          (BTypeFix (BTypeProduct _ (BTypeIndex 0)))
          (BTypeFix (BTypeUntil b (BTypeProduct c (BTypeAngle (BTypeUntil c' (BTypeProduct b' (BTypeIndex 0)))))))
        ) ->
        if generalBTypeCompare b b' && generalBTypeCompare c c' then IFair else Normal
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

getInputType :: BType -> BType
getInputType bType =
  case bType of
    BTypeBox (BTypeFunction (BTypeFix (BTypeProduct input _)) _) -> input
    _ -> error "Should not happen. Error in get input type function"

checkPeano :: [String] -> Bool
checkPeano args = case args of
  [] -> False
  s : ss -> (s == "--peano") || checkPeano ss