--Module to be deleted

module Interpreter.ValueChecker where

{-
import Datatype

isValue :: AExp -> Bool
isValue exp = case exp of
  AExpUnit -> True
  AExpZero -> True
  AExpSuc k -> isValue k
  AExpLambda var t exp' -> True
  AExpProduct exp1 exp2 -> isValue exp1 && isValue exp2
  AExpInl exp' t -> isValue exp'
  AExpInr exp' t -> isValue exp'
  AExpBox exp' -> True
  AExpArrow exp' -> True
  AExpAt exp' -> True
  AExpFix var t exp' -> True
  AExpLocation num -> True
  AExpInto exp' t -> isValue exp'
  AExpNow exp' t -> isValue exp'
  AExpWait exp1 exp2 -> isValue exp1 && isValue exp2
  _ -> False

-}