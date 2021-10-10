module TypeChecker.LimitChecker where

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