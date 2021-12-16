module TypeFunctions.StableChecker where

import Datatype

isStable :: BType -> Bool
isStable bType = case bType of
  BTypeIndex n -> False
  BTypeParametric n tp -> case tp of
    Limit -> False
    Stable -> True
    None -> False
    Both -> True
    CStable -> True
    CBoth -> True
  BTypeNameParam n -> error "Should not happen! Found a typename parameter index when checking whether overall type is stable"
  BTypeUnit -> True
  BTypeNat -> True
  BTypeProduct bt bt' -> (isStable bt) && (isStable bt')
  BTypeSum bt bt' -> (isStable bt) && (isStable bt')
  BTypeFunction bt bt' -> False
  BTypeBox bt -> True
  BTypeAngle bt -> False
  BTypeAt bt -> False
  BTypeFix bt -> False
  BTypeUntil bt bt' -> False
  BTypeBool -> True
