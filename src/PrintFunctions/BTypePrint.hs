module PrintFunctions.BTypePrint where

import Datatype
import TypeFunctions.TypeCompare

printBType :: Integer -> BType -> String
printBType n bType =
  do
    let maybeSpecialBType = findSpecialBType n bType
    case maybeSpecialBType of
      Just s -> s
      _ -> normalPrintBType
  where
    normalPrintBType =
      do
        let currentLevel = bTypeLevel bType
        case bType of
          BTypeIndex ind -> "\'t" ++ show (n -1 - ind)
          BTypeParametric n tp -> "p" ++ show (n)
          BTypeNameParam n -> "p" ++ show (n)
          BTypeUnit -> "Unit"
          BTypeNat -> "Nat"
          BTypeProduct bt bt' ->
            ( if bTypeLevel bt <= currentLevel
                then "(" ++ (printBType n bt) ++ ")"
                else printBType n bt
            )
              ++ " * "
              ++ ( if bTypeLevel bt' <= currentLevel
                     then "(" ++ (printBType n bt') ++ ")"
                     else printBType n bt'
                 )
          BTypeSum bt bt' ->
            ( if bTypeLevel bt <= currentLevel
                then "(" ++ (printBType n bt) ++ ")"
                else printBType n bt
            )
              ++ " + "
              ++ ( if bTypeLevel bt' <= currentLevel
                     then "(" ++ (printBType n bt') ++ ")"
                     else printBType n bt'
                 )
          BTypeFunction bt bt' ->
            ( if bTypeLevel bt <= currentLevel
                then "(" ++ (printBType n bt) ++ ")"
                else printBType n bt
            )
              ++ " -> "
              ++ ( if bTypeLevel bt' < currentLevel
                     then "(" ++ (printBType n bt') ++ ")"
                     else printBType n bt'
                 )
          BTypeBox bt ->
            "#"
              ++ ( if bTypeLevel bt < currentLevel
                     then "(" ++ (printBType n bt) ++ ")"
                     else printBType n bt
                 )
          BTypeAngle bt ->
            ">"
              ++ ( if bTypeLevel bt < currentLevel
                     then "(" ++ (printBType n bt) ++ ")"
                     else printBType n bt
                 )
          BTypeAt bt ->
            "@"
              ++ ( if bTypeLevel bt < currentLevel
                     then "(" ++ (printBType n bt) ++ ")"
                     else printBType n bt
                 )
          BTypeFix bt ->
            "Fix " ++ "\'t" ++ show n ++ " --> "
              ++ printBType (n + 1) bt
          BTypeUntil bt bt' ->
            ( if bTypeLevel bt <= currentLevel
                then "(" ++ (printBType n bt) ++ ")"
                else printBType n bt
            )
              ++ " Until "
              ++ ( if bTypeLevel bt' <= currentLevel
                     then "(" ++ (printBType n bt') ++ ")"
                     else printBType n bt'
                 )
          BTypeBool -> "Bool"

bTypeLevel :: BType -> Integer
bTypeLevel bType =
  case findSpecialBType 0 bType of
    Nothing ->
      case bType of
        BTypeIndex n -> 4
        BTypeParametric n tp -> 4
        BTypeNameParam n -> 4
        BTypeUnit -> 4
        BTypeNat -> 4
        BTypeProduct bt bt' -> 3
        BTypeSum bt bt' -> 2
        BTypeFunction bt bt' -> 0
        BTypeBox bt -> 4
        BTypeAngle bt -> 4
        BTypeAt bt -> 4
        BTypeFix bt -> -1
        BTypeUntil bt bt' -> 1
        BTypeBool -> 4
    _ -> 4

findSpecialBType :: Integer -> BType -> Maybe String
findSpecialBType n bType =
  case bType of
    BTypeFix (BTypeProduct x (BTypeIndex 0)) -> Just ("'Str(" ++ printBType n x ++ ")")
    BTypeFix (BTypeSum x (BTypeIndex 0)) -> Just ("'Ev(" ++ printBType n x ++ ")")
    BTypeFix (BTypeUntil a (BTypeProduct b (BTypeAngle (BTypeUntil b' (BTypeProduct a' (BTypeIndex 0)))))) ->
      if generalBTypeCompare a a' && generalBTypeCompare b b' then Just ("'Fair(" ++ printBType n a ++ ", " ++ printBType n b ++ ")") else Nothing
    BTypeSum BTypeUnit x -> Just ("'Maybe(" ++ printBType n x ++ ")")
    BTypeUntil BTypeUnit x -> Just ("'Dia(" ++ printBType n x ++ ")")
    _ -> Nothing