module ExpTypeConverters.TypeSynonymConverter where

import Data.List
import Datatype
import ExpTypeConverters.TypeNameResolveUtils

-- used in defining a type synonym
typeSynonymConverter :: FilePath -> String -> [String] -> TypenameList -> [String] -> AType -> BType
typeSynonymConverter file typeSynonymName typeVariables definedTypenames varStack aType = case aType of
  ATypeVar s -> resolveTypeVarType file typeSynonymName typeVariables varStack s
  ATypeName s ats -> resolveTypeName file typeSynonymName definedTypenames s (map (typeSynonymConverterCur varStack) ats)
  ATypeUnit -> BTypeUnit
  ATypeNat -> BTypeNat
  ATypeProduct at at' -> BTypeProduct (typeSynonymConverterCur varStack at) (typeSynonymConverterCur varStack at')
  ATypeSum at at' -> BTypeSum (typeSynonymConverterCur varStack at) (typeSynonymConverterCur varStack at')
  ATypeFunction at at' -> BTypeFunction (typeSynonymConverterCur varStack at) (typeSynonymConverterCur varStack at')
  ATypeBox at -> BTypeBox (typeSynonymConverterCur varStack at)
  ATypeAngle at -> BTypeAngle (typeSynonymConverterCur varStack at)
  ATypeAt at -> BTypeAt (typeSynonymConverterCur varStack at)
  ATypeNFix s at -> BTypeNFix (typeSynonymConverterCur (s : varStack) at)
  ATypeUntil at at' -> BTypeUntil (typeSynonymConverterCur varStack at) (typeSynonymConverterCur varStack at')
  ATypeBool -> BTypeBool
  ATypeList at -> BTypeList (typeSynonymConverterCur varStack at)
  where
    typeSynonymConverterCur = typeSynonymConverter file typeSynonymName typeVariables definedTypenames

resolveTypeVarType :: FilePath -> String -> [String] -> [String] -> String -> BType
resolveTypeVarType file typeSynonymName typeVariables varStack str =
  do
    let stackIndex = elemIndex str varStack
    case stackIndex of
      Nothing ->
        do
          let typeVariablesIndex = elemIndex str typeVariables
          case typeVariablesIndex of
            Nothing -> error (file ++ ": " ++ "Cannot resolve the typevariable \"" ++ str ++ "\" with type synonym parameters or NFix variables in \"" ++ typeSynonymName ++ "\"")
            Just n -> BTypeNameParam (toInteger n)
      Just n -> BTypeIndex (toInteger n)
