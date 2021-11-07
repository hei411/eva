module TypeFunctions.SubstituteParametric where

import Datatype

substituteParametric :: BType -> [BType] -> BType
substituteParametric bType typeArguments =
  case bType of
    BTypeIndex n -> BTypeIndex n
    BTypeParametric n tp -> typeArguments !! fromInteger n
    BTypeNameParam n -> error "Should not happen, found typenameparam during substitute parametric "
    BTypeUnit -> BTypeUnit
    BTypeNat -> BTypeNat
    BTypeProduct bt bt' -> BTypeProduct (substituteParametric bt typeArguments) (substituteParametric bt' typeArguments)
    BTypeSum bt bt' -> BTypeSum (substituteParametric bt typeArguments) (substituteParametric bt' typeArguments)
    BTypeFunction bt bt' -> BTypeFunction (substituteParametric bt typeArguments) (substituteParametric bt' typeArguments)
    BTypeBox bt -> BTypeBox (substituteParametric bt typeArguments)
    BTypeAngle bt -> BTypeAngle (substituteParametric bt typeArguments)
    BTypeAt bt -> BTypeAt (substituteParametric bt typeArguments)
    BTypeFix bt -> BTypeFix (substituteParametric bt typeArguments)
    BTypeUntil bt bt' -> BTypeUntil (substituteParametric bt typeArguments) (substituteParametric bt' typeArguments)

{-case typeArguments of
  [] -> bType
  bt : bts -> substituteParametric (index + 1) (substituteOneParametric index bType bt) bts
where
  substituteOneParametric :: Integer -> BType -> BType -> BType
  substituteOneParametric index bType arg = case bType of
    BTypeIndex n -> BTypeIndex n
    BTypeParametric n tp -> if n == index then arg else BTypeParametric n tp
    BTypeNameParam n -> error "Should not happen, found typenameparam during substitute parametric "
    BTypeUnit -> BTypeUnit
    BTypeNat -> BTypeNat
    BTypeProduct bt bt' -> BTypeProduct (substituteOneParametric index bt arg) (substituteOneParametric index bt' arg)
    BTypeSum bt bt' -> BTypeSum (substituteOneParametric index bt arg) (substituteOneParametric index bt' arg)
    BTypeFunction bt bt' -> BTypeFunction (substituteOneParametric index bt arg) (substituteOneParametric index bt' arg)
    BTypeBox bt -> BTypeBox (substituteOneParametric index bt arg)
    BTypeAngle bt -> BTypeAngle (substituteOneParametric index bt arg)
    BTypeAt bt -> BTypeAt (substituteOneParametric index bt arg)
    BTypeFix bt -> BTypeFix (substituteOneParametric index bt arg)
    BTypeUntil bt bt' -> BTypeUntil (substituteOneParametric index bt arg) (substituteOneParametric index bt' arg)
    -}