module ExpTypeConverters.ABTypeConverter where

{-
import Datatype

abTypeConverter :: TypenameList -> AType -> BType
abTypeConverter typenameList aType =
  abTypeConverterHelper [] aType
  where
    abTypeConverterHelper :: [String] -> AType -> BType
    abTypeConverterHelper varStack aType =
      case aType of
        ATypeVar s -> BTypeIndex (findDBIndex varStack s)
        ATypeName s -> findTypename typenameList s
        ATypeUnit -> BTypeUnit
        ATypeNat -> BTypeNat
        ATypeProduct at at' -> BTypeProduct (abTypeConverterHelper varStack at) (abTypeConverterHelper varStack at')
        ATypeSum at at' -> BTypeSum (abTypeConverterHelper varStack at) (abTypeConverterHelper varStack at')
        ATypeFunction at at' -> BTypeFunction (abTypeConverterHelper varStack at) (abTypeConverterHelper varStack at')
        ATypeBox at -> BTypeBox (abTypeConverterHelper varStack at)
        ATypeArrow at -> BTypeArrow (abTypeConverterHelper varStack at)
        ATypeAt at -> BTypeAt (abTypeConverterHelper varStack at)
        ATypeFix s at -> BTypeFix (abTypeConverterHelper (s : varStack) at)
        ATypeUntil at at' -> BTypeUntil (abTypeConverterHelper varStack at) (abTypeConverterHelper varStack at')
        ATypeApplication at at' -> BTypeApplication (abTypeConverterHelper varStack at) (abTypeConverterHelper varStack at')
        ATypeLambda s at -> BTypeLambda (abTypeConverterHelper (s : varStack) at)

findDBIndex :: [String] -> String -> Integer
findDBIndex varStack s =
  case varStack of
    [] -> error ("Type variable " ++ s ++ " in type cannot be resolved")
    hd : tl -> if hd == s then 0 else 1 + findDBIndex tl s

findTypename :: [(String, BType)] -> String -> BType
findTypename typenameList s =
  case typenameList of
    [] -> error ("Typename " ++ s ++ " in type cannot be resolved")
    (typename, bType) : tl -> if typename == s then bType else findTypename tl s

-}