module TypeFunctions.LimitChecker where

import Datatype

isLimit :: BType -> Bool
isLimit bType = case bType of
  BTypeIndex n -> True
  BTypeParametric n tp -> case tp of
    Limit -> True
    Stable -> False
    None -> False
    Both -> True
  BTypeNameParam n -> error "Should not happen! Found a typename parameter index when checking whether overall type is limit"
  BTypeUnit -> True
  BTypeNat -> True
  BTypeProduct bt bt' -> (isLimit bt) && (isLimit bt')
  BTypeSum bt bt' -> (isLimit bt) && (isLimit bt')
  BTypeFunction bt bt' -> isLimit bt'
  BTypeBox bt -> isLimit bt
  BTypeAngle bt -> True
  BTypeAt bt -> isLimit bt
  BTypeFix bt -> isLimit bt
  BTypeUntil bt bt' -> False
