--Module to be MODIFIED

module TypeChecker.StableChecker where

{-
import Datatype

isStable :: BType -> Bool
isStable t = case t of
  ATypeUnit -> True
  ATypeNat -> True
  ATypeBox _ -> True
  ATypeProduct t1 t2 -> (isStable t1) && (isStable t2)
  ATypeSum t1 t2 -> (isStable t1) && (isStable t2)
  _ -> False

-}