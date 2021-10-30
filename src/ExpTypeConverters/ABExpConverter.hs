module ExpTypeConverters.ABExpConverter where

{-
import Datatype
import ExpTypeConverters.ABTypeConverter

abExpConverter :: TypenameList -> AExp -> BExp
abExpConverter typenamesList aExp =
  case aExp of
    AExpVar s -> BExpVar s
    AExpUnit -> BExpUnit
    AExpLambda s at ae -> BExpLambda s (curabTypeConverter at) (curabExpConverter ae)
    AExpApplication ae ae' -> BExpApplication (curabExpConverter ae) (curabExpConverter ae')
    AExpProduct ae ae' -> BExpProduct (curabExpConverter ae) (curabExpConverter ae')
    AExpFst ae -> BExpFst (curabExpConverter ae)
    AExpSnd ae -> BExpSnd (curabExpConverter ae)
    AExpInl ae at -> BExpInl (curabExpConverter ae) (curabTypeConverter at)
    AExpInr ae at -> BExpInr (curabExpConverter ae) (curabTypeConverter at)
    AExpMatch ae s ae' str ae2 -> BExpMatch (curabExpConverter ae) s (curabExpConverter ae') str (curabExpConverter ae2)
    AExpZero -> BExpZero
    AExpSuc ae -> BExpSuc (curabExpConverter ae)
    AExpPrimrec ae ae' s str ae2 -> BExpPrimrec (curabExpConverter ae) (curabExpConverter ae') s str (curabExpConverter ae2)
    AExpArrow ae -> BExpArrow (curabExpConverter ae)
    AExpAt ae -> BExpAt (curabExpConverter ae)
    AExpAdv ae -> BExpAdv (curabExpConverter ae)
    AExpBox ae -> BExpBox (curabExpConverter ae)
    AExpUnbox ae -> BExpUnbox (curabExpConverter ae)
    AExpNow ae at -> BExpNow (curabExpConverter ae) (curabTypeConverter at)
    AExpWait ae ae' -> BExpWait (curabExpConverter ae) (curabExpConverter ae')
    AExpUrec ae s ae' str cs s' ae2 -> BExpUrec (curabExpConverter ae) s (curabExpConverter ae') str cs s' (curabExpConverter ae2)
    AExpFix s at ae -> BExpFix s (curabTypeConverter at) (curabExpConverter ae)
    AExpOut ae -> BExpOut (curabExpConverter ae)
    AExpInto ae at -> BExpInto (curabExpConverter ae) (curabTypeConverter at)
  where
    curabExpConverter = abExpConverter typenamesList
    curabTypeConverter = abTypeConverter typenamesList

-}