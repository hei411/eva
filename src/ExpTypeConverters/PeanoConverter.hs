module ExpTypeConverters.PeanoConverter where

import Datatype

peanoConverterProgram :: Program -> Program
peanoConverterProgram program = case program of
  [] -> []
  state : states -> case state of
    DefStatement s x0 ae -> DefStatement s x0 (peanoConverterAExp ae) : peanoConverterProgram states
    _ -> state : peanoConverterProgram states

peanoConverterAExp :: AExp -> AExp
peanoConverterAExp e = do
  case e of
    AExpVar s ats -> AExpVar s ats
    AExpUnit -> AExpUnit
    AExpLambda s at ae -> AExpLambda s at (peanoConverterAExp ae)
    AExpApplication ae ae' -> AExpApplication (peanoConverterAExp ae) (peanoConverterAExp ae')
    AExpProduct ae ae' -> AExpProduct (peanoConverterAExp ae) (peanoConverterAExp ae')
    AExpFst ae -> AExpFst (peanoConverterAExp ae)
    AExpSnd ae -> AExpSnd (peanoConverterAExp ae)
    AExpInl ae at -> AExpInl (peanoConverterAExp ae) at
    AExpInr ae at -> AExpInr (peanoConverterAExp ae) at
    AExpMatch ae s ae' str ae2 -> AExpMatch (peanoConverterAExp ae) s (peanoConverterAExp ae') str (peanoConverterAExp ae2)
    AExpZero -> AExpZero
    AExpSuc ae -> AExpSuc (peanoConverterAExp ae)
    AExpPrimrec ae ae' s str ae2 -> AExpPrimrec (peanoConverterAExp ae) (peanoConverterAExp ae') s str (peanoConverterAExp ae2)
    AExpAngle ae -> AExpAngle (peanoConverterAExp ae)
    AExpAt ae -> AExpAt (peanoConverterAExp ae)
    AExpAdv ae -> AExpAdv (peanoConverterAExp ae)
    AExpBox ae -> AExpBox (peanoConverterAExp ae)
    AExpUnbox ae -> AExpUnbox (peanoConverterAExp ae)
    AExpNow ae at -> AExpNow (peanoConverterAExp ae) at
    AExpWait ae ae' -> AExpWait (peanoConverterAExp ae) (peanoConverterAExp ae')
    AExpUrec ae s ae' str cs s' ae2 -> AExpUrec (peanoConverterAExp ae) s (peanoConverterAExp ae') str cs s' (peanoConverterAExp ae2)
    AExpRec s at ae -> AExpRec s at (peanoConverterAExp ae)
    AExpOut ae -> AExpOut (peanoConverterAExp ae)
    AExpInto ae at -> AExpInto (peanoConverterAExp ae) at
    AExpLet s ae ae' -> AExpLet s (peanoConverterAExp ae) (peanoConverterAExp ae')
    AExpTrue -> AExpTrue
    AExpFalse -> AExpFalse
    AExpIf ae ae' ae2 -> AExpIf (peanoConverterAExp ae) (peanoConverterAExp ae') (peanoConverterAExp ae2)
    AExpAnd ae ae' -> AExpAnd (peanoConverterAExp ae) (peanoConverterAExp ae')
    AExpOr ae ae' -> AExpOr (peanoConverterAExp ae) (peanoConverterAExp ae')
    AExpNot ae -> AExpNot (peanoConverterAExp ae)
    AExpEquals ae ae' -> AExpEquals (peanoConverterAExp ae) (peanoConverterAExp ae')
    AExpNotEquals ae ae' -> AExpNotEquals (peanoConverterAExp ae) (peanoConverterAExp ae')
    --Special cases
    AExpInteger n -> helper n
    AExpIncrement ae -> AExpSuc (peanoConverterAExp ae)
  where
    helper :: Integer -> AExp
    helper n =
      if n == 0
        then AExpZero
        else AExpSuc (helper (n -1))