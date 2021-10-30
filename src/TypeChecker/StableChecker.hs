--Module to be MODIFIED

module TypeChecker.StableChecker where

{-
import Datatype

isStable :: BType -> Bool
isStable t = case t of
  BTypeIndex n -> False
  BTypeUnit -> True
  BTypeNat -> True
  BTypeProduct bt bt' -> (isStable bt) && (isStable bt')
  BTypeSum bt bt' -> (isStable bt) && (isStable bt')
  BTypeFunction bt bt' -> False
  BTypeBox bt -> True
  BTypeArrow bt -> False
  BTypeAt bt -> False
  BTypeFix bt -> False
  BTypeUntil bt bt' -> False
  BTypeApplication bt bt' -> error "Should not check stability of a BTypeApplication"
  BTypeLambda bt -> error "Should not check stability of a BTypeLambda"
-}