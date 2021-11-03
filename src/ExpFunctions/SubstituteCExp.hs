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
  CExpPrimrec ce ce' ce2 -> CExpPrimrec (substituteCExpHelper ce) (substituteCExpHelper ce) (substituteCExp arg (level + 2) ce')
  CExpDelay ce -> CExpDelay (substituteCExpHelper ce)
  CExpAdv ce -> CExpAdv (substituteCExpHelper ce)
  CExpBox ce -> CExpBox (substituteCExpHelper ce)
  CExpUnbox ce -> CExpUnbox (substituteCExpHelper ce)
  CExpNow ce -> CExpNow (substituteCExpHelper ce)
  CExpWait ce ce' -> CExpWait (substituteCExpHelper ce) (substituteCExpHelper ce')
  CExpUrec ce ce' ce2 -> CExpUrec (substituteCExpHelper ce) (substituteCExp arg (level + 1) ce') (substituteCExp arg (level + 3) ce')
  CExpRec ce -> CExpRec (substituteCExp arg (level + 1) ce)
  CExpOut ce -> CExpOut (substituteCExpHelper ce)
  CExpInto ce -> CExpInto (substituteCExpHelper ce)
  CExpLocation n -> CExpLocation n
  where
    substituteCExpHelper = substituteCExp arg level