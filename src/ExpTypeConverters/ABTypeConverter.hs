module ExpTypeConverters.ABTypeConverter where

import Data.List
import Datatype
import ExpTypeConverters.TypeNameResolveUtils

abTypeConverter :: FilePath -> String -> [(TypeProperty, String)] -> TypenameList -> [String] -> AType -> BType
abTypeConverter file functionName polyParams definedTypenames varStack aType = case aType of
  ATypeVar s -> resolveTypeVarLet file functionName polyParams varStack s
  ATypeName s ats -> resolveTypeName file functionName definedTypenames s (map (abTypeConverterCur varStack) ats)
  ATypeUnit -> BTypeUnit
  ATypeNat -> BTypeNat
  ATypeProduct at at' -> BTypeProduct (abTypeConverterCur varStack at) (abTypeConverterCur varStack at')
  ATypeSum at at' -> BTypeSum (abTypeConverterCur varStack at) (abTypeConverterCur varStack at')
  ATypeFunction at at' -> BTypeFunction (abTypeConverterCur varStack at) (abTypeConverterCur varStack at')
  ATypeBox at -> BTypeBox (abTypeConverterCur varStack at)
  ATypeArrow at -> BTypeArrow (abTypeConverterCur varStack at)
  ATypeAt at -> BTypeAt (abTypeConverterCur varStack at)
  ATypeFix s at -> BTypeFix (abTypeConverterCur (s : varStack) at)
  ATypeUntil at at' -> BTypeUntil (abTypeConverterCur varStack at) (abTypeConverterCur varStack at')
  where
    abTypeConverterCur = abTypeConverter file functionName polyParams definedTypenames

resolveTypeVarLet :: FilePath -> String -> [(TypeProperty, String)] -> [String] -> String -> BType
resolveTypeVarLet file functionName polyParams varStack str =
  do
    let stackIndex = elemIndex str varStack
    case stackIndex of
      Nothing ->
        do
          let polyParamsIndexPair = findPolyParam 0 polyParams str
          case polyParamsIndexPair of
            Nothing -> error (file ++ ": " ++ "Cannot resolve the typevariable \"" ++ str ++ "\" with polymorphic parameters or Fix variables in \"" ++ functionName ++ "\"")
            Just (n, prop) -> BTypeParametric n prop
      Just n -> BTypeIndex (toInteger n)
  where
    findPolyParam :: Integer -> [(TypeProperty, String)] -> String -> Maybe (Integer, TypeProperty)
    findPolyParam currentIndex polyParams str = case polyParams of
      [] -> Nothing
      (prop, name) : tl -> if name == str then Just (currentIndex, prop) else findPolyParam (currentIndex + 1) tl str
