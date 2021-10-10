module TypeChecker.ValidChecker where

import Datatype

isValid :: AType -> Bool
isValid t = isValidHelper t []
  where
    isValidHelper t l = case t of
      ATypeVar s -> elem s l
      ATypeFix s exp -> isValidHelper exp (s : l)
      ATypeProduct x y -> (isValidHelper x l) && (isValidHelper y l)
      ATypeSum x y -> (isValidHelper x l) && (isValidHelper y l)
      ATypeFunction x y -> (isValidHelper x l) && (isValidHelper y l)
      ATypeUntil x y -> (isValidHelper x l) && (isValidHelper y l)
      ATypeBox x -> (isValidHelper x l)
      ATypeArrow x -> (isValidHelper x l)
      ATypeAt x -> (isValidHelper x l)
      _ -> True
