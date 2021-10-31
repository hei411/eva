module ExpTypeConverters.ABTypeConverter where

import Data.List
import Datatype

abTypeConverter :: [(TypeProperty, String)] -> TypenameList -> [String] -> AType -> BType
abTypeConverter polyParams definedTypenames varStack aType = case aType of
  ATypeVar s -> resolveTypeVar polyParams varStack s
  ATypeName s ats -> resolveTypeName definedTypenames s (map (abTypeConverterCur varStack) ats)
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
    abTypeConverterCur = abTypeConverter polyParams definedTypenames

resolveTypeVar :: [(TypeProperty, String)] -> [String] -> String -> BType
resolveTypeVar polyParams varStack str =
  do
    let stackIndex = elemIndex str varStack
    case stackIndex of
      Nothing ->
        do
          let polyParamsIndexPair = findPolyParam 0 polyParams str
          case polyParamsIndexPair of
            Nothing -> error ("Cannot resolve the typevariable " ++ str ++ " with polymorphic parameters or Fix variables.")
            Just (n, prop) -> BTypeParametric n prop
      Just n -> BTypeIndex (toInteger n)
  where
    findPolyParam :: Integer -> [(TypeProperty, String)] -> String -> Maybe (Integer, TypeProperty)
    findPolyParam currentIndex polyParams str = case polyParams of
      [] -> Nothing
      (prop, name) : tl -> if name == str then Just (currentIndex, prop) else findPolyParam (currentIndex + 1) tl str

resolveTypeName :: TypenameList -> String -> [BType] -> BType
resolveTypeName definedTypenames name typeArguments =
  do
    let typenamePair = lookupTypename definedTypenames name
    case typenamePair of
      Nothing -> error ("Cannot resolve the type synonym " ++ name)
      Just (bType, num) ->
        if num /= (toInteger (length typeArguments))
          then error ("Provide wrong number of type arguments to the type synonym " ++ name)
          else foldl (substituteTypenameArg 0) bType typeArguments
  where
    lookupTypename :: TypenameList -> String -> Maybe (BType, Integer)
    lookupTypename definedTypenames name = case definedTypenames of
      [] -> Nothing
      (key, bType, num) : tl -> if key == name then Just (bType, num) else lookupTypename tl name

substituteTypenameArg :: Integer -> BType -> BType -> BType
substituteTypenameArg levelNum bType arg = case bType of
  BTypeIndex n -> BTypeIndex n
  BTypeParametric n tp -> BTypeParametric n tp
  BTypeNameParam n ->
    --really tricky bit. if n == 0 substitute, otherwise subtract one for next typename param
    if n == 0 then promoteFreeVariables levelNum 0 arg else BTypeNameParam (n -1)
  BTypeUnit -> BTypeUnit
  BTypeNat -> BTypeNat
  BTypeProduct bt bt' -> BTypeProduct (substituteTypenameArg levelNum bt arg) (substituteTypenameArg levelNum bt' arg)
  BTypeSum bt bt' -> BTypeSum (substituteTypenameArg levelNum bt arg) (substituteTypenameArg levelNum bt' arg)
  BTypeFunction bt bt' -> BTypeFunction (substituteTypenameArg levelNum bt arg) (substituteTypenameArg levelNum bt' arg)
  BTypeBox bt -> BTypeBox (substituteTypenameArg levelNum bt arg)
  BTypeArrow bt -> BTypeArrow (substituteTypenameArg levelNum bt arg)
  BTypeAt bt -> BTypeAt (substituteTypenameArg levelNum bt arg)
  BTypeFix bt -> BTypeFix (substituteTypenameArg (levelNum + 1) bt arg)
  BTypeUntil bt bt' -> BTypeUntil (substituteTypenameArg levelNum bt arg) (substituteTypenameArg levelNum bt' arg)
  where
    promoteFreeVariables :: Integer -> Integer -> BType -> BType
    promoteFreeVariables levelNum current bType = case bType of
      BTypeIndex n ->
        if n >= current then BTypeIndex (n + levelNum) else BTypeIndex n
      BTypeParametric n tp -> BTypeParametric n tp
      BTypeNameParam n -> error "Not sure, but there should not be any BTypeNameParam in an argument passed to a type synonym, as all arguments should be resolved"
      BTypeUnit -> BTypeUnit
      BTypeNat -> BTypeNat
      BTypeProduct bt bt' -> BTypeProduct (promoteFreeVariablesHelper bt) (promoteFreeVariablesHelper bt')
      BTypeSum bt bt' -> BTypeSum (promoteFreeVariablesHelper bt) (promoteFreeVariablesHelper bt')
      BTypeFunction bt bt' -> BTypeFunction (promoteFreeVariablesHelper bt) (promoteFreeVariablesHelper bt')
      BTypeBox bt -> BTypeBox (promoteFreeVariablesHelper bt)
      BTypeArrow bt -> BTypeArrow (promoteFreeVariablesHelper bt)
      BTypeAt bt -> BTypeAt (promoteFreeVariablesHelper bt)
      BTypeFix bt -> BTypeFix (promoteFreeVariables levelNum (current + 1) bt)
      BTypeUntil bt bt' -> BTypeUntil (promoteFreeVariablesHelper bt) (promoteFreeVariablesHelper bt')
      where
        promoteFreeVariablesHelper = promoteFreeVariables levelNum current
