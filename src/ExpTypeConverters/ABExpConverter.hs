module ExpTypeConverters.ABExpConverter where

import Datatype
import ExpTypeConverters.ABTypeConverter
import Prelude

abExpConverter :: [(TypeProperty, String)] -> TypenameList -> AExp -> BExp
abExpConverter polyParams definedTypenames aExp = case aExp of
  AExpVar s ats -> (BExpVar s (map abTypeConverterCur ats))
  AExpUnit -> BExpUnit
  AExpLambda s at ae -> BExpLambda s (abTypeConverterCur at) (abExpConverterCur ae)
  AExpApplication ae ae' -> BExpApplication (abExpConverterCur ae) (abExpConverterCur ae')
  AExpProduct ae ae' -> BExpProduct (abExpConverterCur ae) (abExpConverterCur ae')
  AExpFst ae -> BExpFst (abExpConverterCur ae)
  AExpSnd ae -> BExpSnd (abExpConverterCur ae)
  AExpInl ae at -> BExpInl (abExpConverterCur ae) (abTypeConverterCur at)
  AExpInr ae at -> BExpInr (abExpConverterCur ae) (abTypeConverterCur at)
  AExpMatch ae s ae' str ae2 -> BExpMatch (abExpConverterCur ae) s (abExpConverterCur ae') str (abExpConverterCur ae2)
  AExpZero -> BExpZero
  AExpSuc ae -> BExpSuc (abExpConverterCur ae)
  AExpPrimrec ae ae' s str ae2 -> BExpPrimrec (abExpConverterCur ae) (abExpConverterCur ae') s str (abExpConverterCur ae2)
  AExpArrow ae -> BExpArrow (abExpConverterCur ae)
  AExpAt ae -> BExpAt (abExpConverterCur ae)
  AExpAdv ae -> BExpAdv (abExpConverterCur ae)
  AExpBox ae -> BExpBox (abExpConverterCur ae)
  AExpUnbox ae -> BExpUnbox (abExpConverterCur ae)
  AExpNow ae at -> BExpNow (abExpConverterCur ae) (abTypeConverterCur at)
  AExpWait ae ae' -> BExpWait (abExpConverterCur ae) (abExpConverterCur ae')
  AExpUrec ae s ae' str cs s' ae2 -> BExpUrec (abExpConverterCur ae) s (abExpConverterCur ae') str cs s' (abExpConverterCur ae2)
  AExpFix s at ae -> BExpFix s (abTypeConverterCur at) (abExpConverterCur ae)
  AExpOut ae -> BExpOut (abExpConverterCur ae)
  AExpInto ae at -> BExpInto (abExpConverterCur ae) (abTypeConverterCur at)
  where
    abExpConverterCur = abExpConverter polyParams definedTypenames
    abTypeConverterCur = (abTypeConverter polyParams definedTypenames [])
