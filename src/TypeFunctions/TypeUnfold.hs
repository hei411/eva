module TypeFunctions.TypeUnfold where

import Datatype

unfoldBType :: BType -> BType
unfoldBType t =
  case t of
    BTypeFix body ->
      -- do not contain special free variables (not including paramtric or typename parameters)
      substituteBType 0 body (BTypeArrow t)
    _ -> error "Should not happen! Applied unfoldBType on a non Fix type"

substituteBType :: Integer -> BType -> BType -> BType
substituteBType targetLevel body arg = case body of
  BTypeIndex n -> if n == targetLevel then arg else BTypeIndex n
  BTypeParametric n tp -> BTypeParametric n tp
  BTypeNameParam n -> BTypeNameParam n
  BTypeUnit -> BTypeUnit
  BTypeNat -> BTypeNat
  BTypeProduct bt bt' -> BTypeProduct (substituteBType targetLevel bt arg) (substituteBType targetLevel bt' arg)
  BTypeSum bt bt' -> BTypeSum (substituteBType targetLevel bt arg) (substituteBType targetLevel bt' arg)
  BTypeFunction bt bt' -> BTypeFunction (substituteBType targetLevel bt arg) (substituteBType targetLevel bt' arg)
  BTypeBox bt -> BTypeBox (substituteBType targetLevel bt arg)
  BTypeArrow bt -> BTypeArrow (substituteBType targetLevel bt arg)
  BTypeAt bt -> BTypeAt (substituteBType targetLevel bt arg)
  BTypeFix bt -> BTypeFix (substituteBType (targetLevel + 1) bt arg)
  BTypeUntil bt bt' -> BTypeUntil (substituteBType targetLevel bt arg) (substituteBType targetLevel bt' arg)

{-
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

-}