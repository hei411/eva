module ExpFunctions.SubstituteCExp where

import Datatype

substituteCExp :: CExp -> Integer -> CExp -> CExp
substituteCExp arg level cExp = case cExp of
  CExpIndex n -> if n == level then arg else cExp
  CExpUnit -> CExpUnit
  CExpLambda ce -> CExpLambda (substituteCExp arg (level + 1) ce)
  CExpApplication ce ce' -> CExpApplication (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpProduct ce ce' -> CExpProduct (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpFst ce -> CExpFst (substituteCExpHelper ce)
  CExpSnd ce -> CExpSnd (substituteCExpHelper ce)
  CExpInl ce -> CExpInl (substituteCExpHelper ce)
  CExpInr ce -> CExpInr (substituteCExpHelper ce)
  CExpMatch ce ce' ce2 -> CExpMatch (substituteCExpHelper ce) (substituteCExp arg (level + 1) ce') (substituteCExp arg (level + 1) ce2)
  CExpZero -> CExpZero
  CExpSuc ce -> CExpSuc (substituteCExpHelper ce)
  CExpPrimrec ce ce' ce2 -> CExpPrimrec (substituteCExpHelper ce) (substituteCExpHelper ce') (substituteCExp arg (level + 2) ce2)
  CExpDelay ce -> CExpDelay (substituteCExpHelper ce)
  CExpAdv ce -> CExpAdv (substituteCExpHelper ce)
  CExpBox ce -> CExpBox (substituteCExpHelper ce)
  CExpUnbox ce -> CExpUnbox (substituteCExpHelper ce)
  CExpNow ce -> CExpNow (substituteCExpHelper ce)
  CExpWait ce ce' -> CExpWait (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpUrec ce ce' ce2 -> CExpUrec (substituteCExpHelper ce) (substituteCExp arg (level + 1) ce') (substituteCExp arg (level + 3) ce2)
  CExpRec ce -> CExpRec (substituteCExp arg (level + 1) ce)
  CExpOut ce -> CExpOut (substituteCExpHelper ce)
  CExpInto ce -> CExpInto (substituteCExpHelper ce)
  CExpLocation n -> CExpLocation n
  CExpTrue -> CExpTrue
  CExpFalse -> CExpFalse
  CExpIf ce ce' ce2 -> CExpIf (substituteCExpHelper ce) (substituteCExpHelper ce') (substituteCExpHelper ce2)
  CExpAnd ce ce' -> CExpAnd (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpOr ce ce' -> CExpOr (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpNot ce -> CExpNot (substituteCExpHelper ce)
  CExpEquals ce ce' -> CExpEquals (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpNotEquals ce ce' -> CExpNotEquals (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpInteger n -> CExpInteger n
  CExpIncrement ce -> CExpIncrement (substituteCExpHelper ce)
  CExpAdd ce ce' -> CExpAdd (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpMinus ce ce' -> CExpMinus (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpMultiply ce ce' -> CExpMultiply (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpDivide ce ce' -> CExpDivide (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpMod ce ce' -> CExpMod (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpPower ce ce' -> CExpPower (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpList ceList -> CExpList (map substituteCExpHelper ceList)
  CExpListAppend ce ce' -> CExpListAppend (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpListCons ce ce' -> CExpListCons (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpListRec ce ce' ce2 -> CExpListRec (substituteCExpHelper ce) (substituteCExpHelper ce') (substituteCExp arg (level + 3) ce2)
  where
    substituteCExpHelper = substituteCExp arg level