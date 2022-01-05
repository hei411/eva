module TypeFunctions.TypePropertyChecker where

import Datatype
import TypeFunctions.ComparableChecker
import TypeFunctions.LimitChecker
import TypeFunctions.StableChecker

checkTypeProperty :: [TypeProperty] -> [BType] -> Either () (BType, TypeProperty)
checkTypeProperty tps ts =
  case (tps, ts) of
    ([], []) -> Left ()
    (tp : tpsTail, t : tsTail) -> case tp of
      Limit -> if isLimit t then checkTypeProperty tpsTail tsTail else Right (t, Limit)
      Stable -> if isStable t then checkTypeProperty tpsTail tsTail else Right (t, Stable)
      None -> checkTypeProperty tpsTail tsTail
      LimitStable -> if isStable t && isLimit t then checkTypeProperty tpsTail tsTail else Right (t, LimitStable)
      Comparable -> if isComparable t then checkTypeProperty tpsTail tsTail else Right (t, Comparable)
    _ -> error "Should not happen! Two list arguments for checkTypeProperty not equal in length"