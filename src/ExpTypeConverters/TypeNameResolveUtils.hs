module ExpTypeConverters.TypeNameResolveUtils where

import Datatype

-- utils for finding correct type name
resolveTypeName :: FilePath -> String -> TypenameList -> String -> [BType] -> BType
resolveTypeName file functionOrTypeSynonymName definedTypenames name typeArguments =
  do
    let typenamePair = lookupTypename definedTypenames name
    case typenamePair of
      Nothing -> error (file ++ ": " ++ "Cannot resolve the type synonym \"" ++ name ++ "\" in \"" ++ functionOrTypeSynonymName ++ "\"")
      Just (bType, num) ->
        if num /= (toInteger (length typeArguments))
          then error (file ++ ": " ++ "Provide wrong number of type arguments to the type synonym \"" ++ name ++ "\" in \"" ++ functionOrTypeSynonymName ++ "\"")
          else substituteTypenameArg 0 bType typeArguments
  where
    lookupTypename :: TypenameList -> String -> Maybe (BType, Integer)
    lookupTypename definedTypenames name = case definedTypenames of
      [] -> Nothing
      (key, bType, num) : tl -> if key == name then Just (bType, num) else lookupTypename tl name

substituteTypenameArg :: Integer -> BType -> [BType] -> BType
substituteTypenameArg levelNum bType arg = case bType of
  BTypeIndex n -> BTypeIndex n
  BTypeParametric n tp -> BTypeParametric n tp
  BTypeNameParam n ->
    arg !! (fromInteger (n))
  BTypeUnit -> BTypeUnit
  BTypeNat -> BTypeNat
  BTypeProduct bt bt' -> BTypeProduct (substituteTypenameArg levelNum bt arg) (substituteTypenameArg levelNum bt' arg)
  BTypeSum bt bt' -> BTypeSum (substituteTypenameArg levelNum bt arg) (substituteTypenameArg levelNum bt' arg)
  BTypeFunction bt bt' -> BTypeFunction (substituteTypenameArg levelNum bt arg) (substituteTypenameArg levelNum bt' arg)
  BTypeBox bt -> BTypeBox (substituteTypenameArg levelNum bt arg)
  BTypeAngle bt -> BTypeAngle (substituteTypenameArg levelNum bt arg)
  BTypeAt bt -> BTypeAt (substituteTypenameArg levelNum bt arg)
  BTypeFix bt -> BTypeFix (substituteTypenameArg (levelNum + 1) bt arg)
  BTypeUntil bt bt' -> BTypeUntil (substituteTypenameArg levelNum bt arg) (substituteTypenameArg levelNum bt' arg)
  BTypeBool -> BTypeBool
  BTypeList bt -> BTypeList (substituteTypenameArg levelNum bt arg)

{-where
  promoteFreeVariables :: Integer -> Integer -> BType -> BType
  promoteFreeVariables levelNum current bType = case bType of
    BTypeIndex n ->
      if n >= current then BTypeIndex (n + levelNum) else BTypeIndex n
    BTypeParametric n tp -> BTypeParametric n tp
    BTypeNameParam n -> BTypeNameParam n
    BTypeUnit -> BTypeUnit
    BTypeNat -> BTypeNat
    BTypeProduct bt bt' -> BTypeProduct (promoteFreeVariablesHelper bt) (promoteFreeVariablesHelper bt')
    BTypeSum bt bt' -> BTypeSum (promoteFreeVariablesHelper bt) (promoteFreeVariablesHelper bt')
    BTypeFunction bt bt' -> BTypeFunction (promoteFreeVariablesHelper bt) (promoteFreeVariablesHelper bt')
    BTypeBox bt -> BTypeBox (promoteFreeVariablesHelper bt)
    BTypeAngle bt -> BTypeAngle (promoteFreeVariablesHelper bt)
    BTypeAt bt -> BTypeAt (promoteFreeVariablesHelper bt)
    BTypeFix bt -> BTypeFix (promoteFreeVariables levelNum (current + 1) bt)
    BTypeUntil bt bt' -> BTypeUntil (promoteFreeVariablesHelper bt) (promoteFreeVariablesHelper bt')
    BTypeBool -> BTypeBool
    where
      promoteFreeVariablesHelper = promoteFreeVariables levelNum current-}
