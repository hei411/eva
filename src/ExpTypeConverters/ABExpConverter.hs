module ExpTypeConverters.ABExpConverter where

import Datatype
import ExpTypeConverters.ABTypeConverter
import Prelude

abExpConverter :: FilePath -> String -> [(TypeProperty, String)] -> TypenameList -> AExp -> BExp
abExpConverter file functionName polyParams definedTypenames aExp = case aExp of
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
  AExpAngle ae -> BExpAngle (abExpConverterCur ae)
  AExpAt ae -> BExpAt (abExpConverterCur ae)
  AExpAdv ae -> BExpAdv (abExpConverterCur ae)
  AExpBox ae -> BExpBox (abExpConverterCur ae)
  AExpUnbox ae -> BExpUnbox (abExpConverterCur ae)
  AExpNow ae at -> BExpNow (abExpConverterCur ae) (abTypeConverterCur at)
  AExpWait ae ae' -> BExpWait (abExpConverterCur ae) (abExpConverterCur ae')
  AExpUrec ae s ae' str cs s' ae2 -> BExpUrec (abExpConverterCur ae) s (abExpConverterCur ae') str cs s' (abExpConverterCur ae2)
  AExpRec s at ae -> BExpRec s (abTypeConverterCur at) (abExpConverterCur ae)
  AExpOut ae -> BExpOut (abExpConverterCur ae)
  AExpInto ae at -> BExpInto (abExpConverterCur ae) (abTypeConverterCur at)
  AExpLet s ae ae' -> BExpLet s (abExpConverterCur ae) (abExpConverterCur ae')
  AExpTrue -> BExpTrue
  AExpFalse -> BExpFalse
  AExpIf ae ae' ae2 -> BExpIf (abExpConverterCur ae) (abExpConverterCur ae') (abExpConverterCur ae2)
  AExpAnd ae ae' -> BExpAnd (abExpConverterCur ae) (abExpConverterCur ae')
  AExpOr ae ae' -> BExpOr (abExpConverterCur ae) (abExpConverterCur ae')
  AExpNot ae -> BExpNot (abExpConverterCur ae)
  AExpEquals ae ae' -> BExpEquals (abExpConverterCur ae) (abExpConverterCur ae')
  AExpNotEquals ae ae' -> BExpNotEquals (abExpConverterCur ae) (abExpConverterCur ae')
  AExpInteger n -> BExpInteger n
  AExpIncrement ae -> BExpIncrement (abExpConverterCur ae)
  AExpAdd ae ae' -> BExpAdd (abExpConverterCur ae) (abExpConverterCur ae')
  AExpMinus ae ae' -> BExpMinus (abExpConverterCur ae) (abExpConverterCur ae')
  AExpMultiply ae ae' -> BExpMultiply (abExpConverterCur ae) (abExpConverterCur ae')
  AExpDivide ae ae' -> BExpDivide (abExpConverterCur ae) (abExpConverterCur ae')
  AExpMod ae ae' -> BExpMod (abExpConverterCur ae) (abExpConverterCur ae')
  AExpPower ae ae' -> BExpPower (abExpConverterCur ae) (abExpConverterCur ae')
  where
    abExpConverterCur = abExpConverter file functionName polyParams definedTypenames
    abTypeConverterCur = (abTypeConverter file functionName polyParams definedTypenames [])
