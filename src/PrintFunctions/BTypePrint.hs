module PrintFunctions.BTypePrint where

import Datatype

printBType :: Integer -> BType -> String
printBType n bType =
  do
    let currentLevel = bTypeLevel bType
    case bType of
      BTypeIndex ind -> "\'" ++ show (n -1 - ind)
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
        "Fix " ++ "\'" ++ show n ++ " -> "
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

bTypeLevel :: BType -> Integer
bTypeLevel bType = case bType of
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
