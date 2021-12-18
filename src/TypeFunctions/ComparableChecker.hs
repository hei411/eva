module TypeFunctions.ComparableChecker where

import Datatype

isComparable :: BType -> Bool
isComparable bType = case bType of
  BTypeIndex n -> False
  BTypeParametric n tp -> case tp of
    Limit -> False
    Stable -> False
    None -> False
    Both -> False
    CStable -> True
    CBoth -> True
  BTypeNameParam n -> error "Should not happen! Found a typename parameter index when checking whether overall type is comparable"
  BTypeUnit -> True
  BTypeNat -> True
  BTypeProduct bt bt' -> (isComparable bt) && (isComparable bt')
  BTypeSum bt bt' -> (isComparable bt) && (isComparable bt')
  BTypeFunction bt bt' -> False
  BTypeBox bt -> False
  BTypeAngle bt -> False
  BTypeAt bt -> False
  BTypeNFix bt -> False
  BTypeUntil bt bt' -> False
  BTypeBool -> True
  BTypeList bt -> (isComparable bt)
