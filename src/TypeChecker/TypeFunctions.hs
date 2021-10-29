--Module to be deleted

module TypeChecker.TypeFunctions where

{-
import Datatype

position :: String -> [String] -> Integer
position v l = case l of
  h : t -> (if h == v then 0 else 1 + (position v t))
  [] -> error "Should not happen since all type ascriptions are correct!"

areEqualTypes :: AType -> AType -> Bool
areEqualTypes t1 t2 = areEqualTypesHelper t1 [] t2 []
  where
    areEqualTypesHelper t1 l1 t2 l2 =
      case (t1, t2) of
        (ATypeVar v1, ATypeVar v2) -> position v1 l1 == position v2 l2
        (ATypeUnit, ATypeUnit) -> True
        (ATypeNat, ATypeNat) -> True
        (ATypeProduct v1 v1', ATypeProduct v2 v2') -> areEqualTypesHelper v1 l1 v2 l2 && areEqualTypesHelper v1' l1 v2' l2
        (ATypeSum v1 v1', ATypeSum v2 v2') -> areEqualTypesHelper v1 l1 v2 l2 && areEqualTypesHelper v1' l1 v2' l2
        (ATypeFunction v1 v1', ATypeFunction v2 v2') -> areEqualTypesHelper v1 l1 v2 l2 && areEqualTypesHelper v1' l1 v2' l2
        (ATypeBox v1, ATypeBox v2) -> areEqualTypesHelper v1 l1 v2 l2
        (ATypeArrow v1, ATypeArrow v2) -> areEqualTypesHelper v1 l1 v2 l2
        (ATypeAt v1, ATypeAt v2) -> areEqualTypesHelper v1 l1 v2 l2
        (ATypeUntil v1 v1', ATypeUntil v2 v2') -> areEqualTypesHelper v1 l1 v2 l2 && areEqualTypesHelper v1' l1 v2' l2
        (ATypeFix s1 v1, ATypeFix s2 v2) -> areEqualTypesHelper v1 (s1 : l1) v2 (s2 : l2)
        _ -> False

substituteType :: AType -> String -> AType -> AType
substituteType t1 s t2 = case t1 of
  ATypeVar str -> if str == s then t2 else ATypeVar str
  ATypeUnit -> ATypeUnit
  ATypeNat -> ATypeNat
  ATypeProduct at at' -> ATypeProduct (substituteType at s t2) (substituteType at' s t2)
  ATypeSum at at' -> ATypeSum (substituteType at s t2) (substituteType at' s t2)
  ATypeFunction at at' -> ATypeFunction (substituteType at s t2) (substituteType at' s t2)
  ATypeBox at -> ATypeBox (substituteType at s t2)
  ATypeArrow at -> ATypeArrow (substituteType at s t2)
  ATypeAt at -> ATypeAt (substituteType at s t2)
  ATypeFix str at -> if str == s then ATypeFix str at else ATypeFix str (substituteType at s t2)
  ATypeUntil at at' -> ATypeUntil (substituteType at s t2) (substituteType at' s t2)
  ATypeApplication at at' -> ATypeApplication (substituteType at s t2) (substituteType at' s t2)

fixUnfold :: AType -> Maybe AType
fixUnfold t =
  case t of
    ATypeFix a a' ->
      return (substituteType a' a (ATypeArrow t))
    _ -> Nothing

-}