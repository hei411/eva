module AscriptionSimplifier.AscriptionSimplifier where

import Datatype
import TypeFunctions.TypeSubstitute

simplifyBExp :: BExp -> BExp
simplifyBExp bExp = case bExp of
  BExpVar s -> BExpVar s
  BExpUnit -> BExpUnit
  BExpLambda s bt be -> BExpLambda s (simplifyBType bt) (simplifyBExp be)
  BExpApplication be be' -> BExpApplication (simplifyBExp be) (simplifyBExp be')
  BExpProduct be be' -> BExpProduct (simplifyBExp be) (simplifyBExp be')
  BExpFst be -> BExpFst (simplifyBExp be)
  BExpSnd be -> BExpSnd (simplifyBExp be)
  BExpInl be bt -> BExpInl (simplifyBExp be) (simplifyBType bt)
  BExpInr be bt -> BExpInr (simplifyBExp be) (simplifyBType bt)
  BExpMatch be s be' str be2 -> BExpMatch (simplifyBExp be) s (simplifyBExp be') str (simplifyBExp be2)
  BExpZero -> BExpZero
  BExpSuc be -> BExpSuc (simplifyBExp be)
  BExpPrimrec be be' s str be2 -> BExpPrimrec (simplifyBExp be) (simplifyBExp be') s str (simplifyBExp be2)
  BExpArrow be -> BExpArrow (simplifyBExp be)
  BExpAt be -> BExpAt (simplifyBExp be)
  BExpAdv be -> BExpAdv (simplifyBExp be)
  BExpBox be -> BExpBox (simplifyBExp be)
  BExpUnbox be -> BExpUnbox (simplifyBExp be)
  BExpNow be bt -> BExpNow (simplifyBExp be) (simplifyBType bt)
  BExpWait be be' -> BExpWait (simplifyBExp be) (simplifyBExp be')
  BExpUrec be s be' str cs s' be2 -> BExpUrec (simplifyBExp be) s (simplifyBExp be') str cs s' (simplifyBExp be2)
  BExpFix s bt be -> BExpFix s (simplifyBType bt) (simplifyBExp be)
  BExpOut be -> BExpOut (simplifyBExp be)
  BExpInto be bt -> BExpInto (simplifyBExp be) (simplifyBType bt)

simplifyBType :: BType -> BType
simplifyBType bType = case bType of
  BTypeIndex n -> BTypeIndex n
  BTypeUnit -> BTypeUnit
  BTypeNat -> BTypeNat
  BTypeProduct bt bt' -> BTypeProduct (simplifyBType bt) (simplifyBType bt')
  BTypeSum bt bt' -> BTypeSum (simplifyBType bt) (simplifyBType bt')
  BTypeFunction bt bt' -> BTypeFunction (simplifyBType bt) (simplifyBType bt')
  BTypeBox bt -> BTypeBox (simplifyBType bt)
  BTypeArrow bt -> BTypeArrow (simplifyBType bt)
  BTypeAt bt -> BTypeAt (simplifyBType bt)
  BTypeFix bt -> BTypeFix (simplifyBType bt)
  BTypeUntil bt bt' -> BTypeUntil (simplifyBType bt) (simplifyBType bt')
  BTypeApplication bt bt' ->
    do
      let simplifiedArg = simplifyBType bt'
      case bt of
        BTypeLambda body ->
          simplifyBType (substituteBType body 0 simplifiedArg)
        _ -> BTypeApplication (simplifyBType bt) simplifiedArg
  BTypeLambda bt -> BTypeLambda (simplifyBType bt)
