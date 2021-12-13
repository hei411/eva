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
      Both -> if isStable t && isLimit t then checkTypeProperty tpsTail tsTail else Right (t, Both)
      CStable -> if isComparable t then checkTypeProperty tpsTail tsTail else Right (t, CStable)
      CBoth -> if isComparable t && isLimit t then checkTypeProperty tpsTail tsTail else Right (t, Both)
    _ -> error "Should not happen! Two list arguments for checkTypeProperty not equal in length"