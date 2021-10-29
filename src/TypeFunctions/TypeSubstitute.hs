module TypeFunctions.TypeSubstitute where

import Datatype

substituteBType :: BType -> Integer -> BType ->BType 
substituteBType body index bType  = case body of
  BTypeIndex n -> if n==index then bType else if n>index then BTypeIndex (n-1) else BTypeIndex n
  BTypeUnit -> BTypeUnit
  BTypeNat -> BTypeNat
  BTypeProduct bt bt' -> BTypeProduct (substituteBType bt index bType) (substituteBType bt' index bType)
  BTypeSum bt bt' -> BTypeSum (substituteBType bt index bType) (substituteBType bt' index bType)
  BTypeFunction bt bt' -> BTypeFunction (substituteBType bt index bType) (substituteBType bt' index bType)
  BTypeBox bt -> BTypeBox (substituteBType bt index bType) 
  BTypeArrow bt ->  BTypeArrow (substituteBType bt index bType) 
  BTypeAt bt ->  BTypeAt (substituteBType bt index bType) 
  BTypeFix bt -> BTypeFix (substituteBType bt (index+1) bType) 
  BTypeUntil bt bt' -> BTypeUntil (substituteBType bt index bType) (substituteBType bt' index bType)
  BTypeApplication bt bt' -> BTypeApplication (substituteBType bt index bType) (substituteBType bt' index bType)
  BTypeLambda bt -> BTypeLambda (substituteBType bt (index+1) bType) 