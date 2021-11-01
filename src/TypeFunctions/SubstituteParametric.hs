module TypeFunctions.SubstituteParametric where

import Datatype

substituteParametric :: Integer -> BType -> [BType] -> BType
substituteParametric index bType typeArguments = case typeArguments of
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
      BTypeArrow bt -> BTypeArrow (substituteOneParametric index bt arg)
      BTypeAt bt -> BTypeAt (substituteOneParametric index bt arg)
      BTypeFix bt -> BTypeFix (substituteOneParametric index bt arg)
      BTypeUntil bt bt' -> BTypeUntil (substituteOneParametric index bt arg) (substituteOneParametric index bt' arg)