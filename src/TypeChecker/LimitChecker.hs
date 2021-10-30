--module to be modified
module TypeChecker.LimitChecker where

import Datatype

isLimit :: BType -> Bool
isLimit t = case t of
  BTypeIndex n -> True
  BTypeUnit -> True
  BTypeNat -> True
  BTypeProduct bt bt' -> (isLimit bt) && (isLimit bt')
  BTypeSum bt bt' -> (isLimit bt) && (isLimit bt')
  BTypeFunction bt bt' -> isLimit bt'
  BTypeBox bt -> isLimit bt
  BTypeArrow bt -> True
  BTypeAt bt -> isLimit bt
  BTypeFix bt -> isLimit bt
  BTypeUntil bt bt' -> False
  BTypeApplication bt bt' -> error "Should not check limit of a BTypeApplication"
  BTypeLambda bt -> error "Should not check limit of a BTypeLambda"

{-
import Datatype

isLimit :: AType -> Bool
isLimit t = case t of
  ATypeVar _ -> True
  ATypeUnit -> True
  ATypeNat -> True
  ATypeArrow _ -> True
  ATypeAt x -> isLimit x
  ATypeBox x -> isLimit x
  ATypeProduct x y -> (isLimit x) && (isLimit y)
  ATypeSum x y -> (isLimit x) && (isLimit y)
  ATypeFunction x y -> isLimit y
  ATypeFix x y -> isLimit y
  _ -> False

-}