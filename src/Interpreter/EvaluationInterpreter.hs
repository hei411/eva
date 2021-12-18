-- Module to be modified

module Interpreter.EvaluationInterpreter where

import Control.Exception (evaluate)
import Datatype
import ExpFunctions.SubstituteCExp
import Interpreter.StoreFunctions

evaluationInterpreter :: CExp -> Store -> (CExp, Store)
evaluationInterpreter exp store = case exp of
  CExpIndex n -> error "Should not happen, evaluation interpreter called on a variable"
  CExpUnit -> (CExpUnit, store)
  CExpLambda ce -> (CExpLambda ce, store)
  CExpApplication ce ce' -> applicationEval ce ce' store
  CExpProduct ce ce' -> productEval ce ce' store
  CExpFst ce -> fstEval ce store
  CExpSnd ce -> sndEval ce store
  CExpInl ce -> inlEval ce store
  CExpInr ce -> inrEval ce store
  CExpMatch ce ce' ce2 -> matchEval ce ce' ce2 store
  CExpZero -> (CExpZero, store)
  CExpSuc ce -> sucEval ce store
  CExpPrimrec ce ce' ce2 -> primrecEval ce ce' ce2 store
  CExpDelay ce -> delayEval ce store
  CExpAdv ce -> advEval ce store
  CExpBox ce -> (CExpBox ce, store)
  CExpUnbox ce -> unboxEval ce store
  CExpNow ce -> nowEval ce store
  CExpWait ce ce' -> waitEval ce ce' store
  CExpUrec ce ce' ce2 -> urecEval ce ce' ce2 store
  CExpRec ce -> (CExpRec ce, store)
  CExpOut ce -> outEval ce store
  CExpInto ce -> intoEval ce store
  CExpLocation n -> (CExpLocation n, store)
  CExpTrue -> (CExpTrue, store)
  CExpFalse -> (CExpFalse, store)
  CExpIf ce ce' ce2 -> ifEval ce ce' ce2 store
  CExpAnd ce ce' -> andEval ce ce' store
  CExpOr ce ce' -> orEval ce ce' store
  CExpNot ce -> notEval ce store
  CExpEquals ce ce' -> equalsEval ce ce' store
  CExpNotEquals ce ce' -> notEqualsEval ce ce' store
  CExpInteger n -> (CExpInteger n, store)
  CExpIncrement ce -> incrementEval ce store
  CExpAdd ce ce' -> addEval ce ce' store
  CExpMinus ce ce' -> minusEval ce ce' store
  CExpMultiply ce ce' -> multiplyEval ce ce' store
  CExpDivide ce ce' -> divideEval ce ce' store
  CExpMod ce ce' -> modEval ce ce' store
  CExpPower ce ce' -> powerEval ce ce' store
  CExpList ceList -> listEval ceList store
  CExpListAppend ce ce' -> listAppendEval ce ce' store
  CExpListCons ce ce' -> listConsEval ce ce' store
  CExpListRec ce ce' ce2 -> listRecEval ce ce' ce2 store

applicationEval :: CExp -> CExp -> Store -> (CExp, Store)
applicationEval e1 e2 s = do
  let (lambda, s') = evaluationInterpreter e1 s
  let (v, s'') = evaluationInterpreter e2 s'
  case lambda of
    CExpLambda body ->
      do
        let newBody = substituteCExp v 0 body
        evaluationInterpreter newBody s''
    _ -> error "Should not happen! application of expressions applied to a non-lambda abstraction"

productEval :: CExp -> CExp -> Store -> (CExp, Store)
productEval e1 e2 s = do
  let (v1, s') = evaluationInterpreter e1 s
  let (v2, s'') = evaluationInterpreter e2 s'
  (CExpProduct v1 v2, s'')

fstEval :: CExp -> Store -> (CExp, Store)
fstEval e s = do
  let (e', s') = evaluationInterpreter e s
  case e' of
    CExpProduct a _ -> (a, s')
    _ -> error "Should not happen! fst applied to a non product expression"

sndEval :: CExp -> Store -> (CExp, Store)
sndEval e s = do
  let (e', s') = evaluationInterpreter e s
  case e' of
    CExpProduct _ b -> (b, s')
    _ -> error "Should not happen! snd applied to a non product expression"

inlEval :: CExp -> Store -> (CExp, Store)
inlEval e s = do
  let (e', s') = evaluationInterpreter e s
  (CExpInl e', s')

inrEval :: CExp -> Store -> (CExp, Store)
inrEval e s = do
  let (e', s') = evaluationInterpreter e s
  (CExpInr e', s')

matchEval :: CExp -> CExp -> CExp -> Store -> (CExp, Store)
matchEval e e1 e2 s = do
  let (e', s') = evaluationInterpreter e s
  case e' of
    CExpInl a -> do
      let e1' = substituteCExp a 0 e1
      evaluationInterpreter e1' s'
    CExpInr b -> do
      let e2' = substituteCExp b 0 e2
      evaluationInterpreter e2' s'
    _ -> error "Should not happen! match applied to a non sum expression"

sucEval :: CExp -> Store -> (CExp, Store)
sucEval e s =
  do
    let (e', s') = evaluationInterpreter e s
    (CExpSuc e', s')

primrecEval :: CExp -> CExp -> CExp -> Store -> (CExp, Store)
primrecEval e e1 e2 s =
  do
    let (e', s') = evaluationInterpreter e s
    case e' of
      CExpZero -> evaluationInterpreter e1 s'
      CExpSuc pred -> do
        let nextExp = CExpPrimrec pred e1 e2
        let (nextValue, s'') = evaluationInterpreter nextExp s'
        let e2' = substituteCExp nextValue 0 e2
        let e2'' = substituteCExp pred 1 e2'
        evaluationInterpreter e2'' s''
      CExpInteger n -> do
        if n == 0
          then evaluationInterpreter e1 s'
          else do
            let pred = CExpInteger (n -1)
            let nextExp = CExpPrimrec pred e1 e2
            let (nextValue, s'') = evaluationInterpreter nextExp s'
            let e2' = substituteCExp nextValue 0 e2
            let e2'' = substituteCExp pred 1 e2'
            evaluationInterpreter e2'' s''
      _ -> error "Should not happen! primrec applied to a non nat expression"

delayEval :: CExp -> Store -> (CExp, Store)
delayEval e s = do
  let (s', location) = addStoreElem s e
  (location, s')

advEval :: CExp -> Store -> (CExp, Store)
advEval e s = case s of
  NullStore -> error "Should not happen! adv applied to a nullstore"
  TicklessStore x0 -> error "Should not happen! adv applied to a tickless store"
  TickStore sN sL -> do
    ---Wno-incomplete-uni-patterns
    let (e', TicklessStore expList) = evaluationInterpreter e (TicklessStore sN)
    case e' of
      CExpLocation n -> do
        let e'' = elemStore expList n
        evaluationInterpreter e'' (TickStore expList sL)
      _ -> error "Should not happen! adv expression doesnt produce a location"

unboxEval :: CExp -> Store -> (CExp, Store)
unboxEval e s = do
  case s of
    NullStore -> error ("Should not happen! unbox applied in a nullstore")
    _ -> do
      let (e', s') = evaluationInterpreter e NullStore
      case e' of
        CExpBox body ->
          evaluationInterpreter body s
        CExpRec body -> do
          let arg = CExpBox (CExpDelay (CExpUnbox e'))
          let e'' = substituteCExp arg 0 body
          evaluationInterpreter e'' s
        _ -> error "Should not happen! unbox applied to a non box/rec expression"

nowEval :: CExp -> Store -> (CExp, Store)
nowEval e s = do
  let (e', s') = evaluationInterpreter e s
  (CExpNow e', s')

waitEval :: CExp -> CExp -> Store -> (CExp, Store)
waitEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  (CExpWait e1' e2', s'')

urecEval :: CExp -> CExp -> CExp -> Store -> (CExp, Store)
urecEval e e1 e2 s = do
  let (e', s') = evaluationInterpreter e s
  case e' of
    CExpNow v -> do
      let e1' = substituteCExp v 0 e1
      evaluationInterpreter e1' s'
    CExpWait v1 v2 -> do
      let nextExp = CExpUrec (CExpAdv v2) e1 e2
      let (s'', location) = addStoreElem s' nextExp
      let e2_one = substituteCExp location 0 e2
      let e2_two = substituteCExp v2 1 e2_one
      let e2_three = substituteCExp v1 2 e2_two
      evaluationInterpreter e2_three s''
    _ -> error "Should not happen! urec applied to a non- until expression"

outEval :: CExp -> Store -> (CExp, Store)
outEval e s = do
  let (e', s') = evaluationInterpreter e s
  case e' of
    CExpInto v -> (v, s')
    _ -> error "Should not happen! out applied to a non into expression"

intoEval :: CExp -> Store -> (CExp, Store)
intoEval e s = do
  let (e', s') = evaluationInterpreter e s
  (CExpInto e', s')

ifEval :: CExp -> CExp -> CExp -> Store -> (CExp, Store)
ifEval e e1 e2 s = do
  let (e', s') = evaluationInterpreter e s
  case e' of
    CExpTrue -> evaluationInterpreter e1 s'
    CExpFalse -> evaluationInterpreter e2 s'
    _ -> error "Should not happen! if applied to a non-boolean expression"

andEval :: CExp -> CExp -> Store -> (CExp, Store)
andEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case (e1', e2') of
    (CExpTrue, CExpTrue) -> (CExpTrue, s'')
    (CExpTrue, CExpFalse) -> (CExpFalse, s'')
    (CExpFalse, CExpTrue) -> (CExpFalse, s'')
    (CExpFalse, CExpFalse) -> (CExpFalse, s'')
    _ -> error "Should not happen! and applied to a non-boolean argument"

orEval :: CExp -> CExp -> Store -> (CExp, Store)
orEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case (e1', e2') of
    (CExpTrue, CExpTrue) -> (CExpTrue, s'')
    (CExpTrue, CExpFalse) -> (CExpTrue, s'')
    (CExpFalse, CExpTrue) -> (CExpTrue, s'')
    (CExpFalse, CExpFalse) -> (CExpFalse, s'')
    _ -> error "Should not happen! or applied to a non-boolean argument"

notEval :: CExp -> Store -> (CExp, Store)
notEval e s = do
  let (e', s') = evaluationInterpreter e s
  case e' of
    CExpTrue -> (CExpFalse, s')
    CExpFalse -> (CExpTrue, s')
    _ -> error "Should not happen! not applied to a non-boolean argument"

equalsEval :: CExp -> CExp -> Store -> (CExp, Store)
equalsEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  if e1' == e2'
    then (CExpTrue, s'')
    else (CExpFalse, s'')

notEqualsEval :: CExp -> CExp -> Store -> (CExp, Store)
notEqualsEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  if e1' /= e2'
    then (CExpTrue, s'')
    else (CExpFalse, s'')

incrementEval :: CExp -> Store -> (CExp, Store)
incrementEval e s = do
  let (e', s') = evaluationInterpreter e s
  case e' of
    CExpInteger n ->
      (CExpInteger (n + 1), s')
    _ -> error "Should not happen! increment applied to a non-integer argument"

addEval :: CExp -> CExp -> Store -> (CExp, Store)
addEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case (e1', e2') of
    --Nonpeano
    (CExpInteger n1, CExpInteger n2) ->
      (CExpInteger (n1 + n2), s'')
    --peano
    _ -> do
      let n1 = peanoToInteger e1'
      let n2 = peanoToInteger e2'
      (integerToPeano (n1 + n2), s'')

minusEval :: CExp -> CExp -> Store -> (CExp, Store)
minusEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case (e1', e2') of
    --Nonpeano
    (CExpInteger n1, CExpInteger n2) ->
      (CExpInteger (max 0 (n1 - n2)), s'')
    --peano
    _ -> do
      let n1 = peanoToInteger e1'
      let n2 = peanoToInteger e2'
      ((integerToPeano (max 0 (n1 - n2))), s'')

multiplyEval :: CExp -> CExp -> Store -> (CExp, Store)
multiplyEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case (e1', e2') of
    --Nonpeano
    (CExpInteger n1, CExpInteger n2) ->
      (CExpInteger (n1 * n2), s'')
    --peano
    _ -> do
      let n1 = peanoToInteger e1'
      let n2 = peanoToInteger e2'
      (integerToPeano (n1 * n2), s'')

divideEval :: CExp -> CExp -> Store -> (CExp, Store)
divideEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case (e1', e2') of
    --Nonpeano
    (CExpInteger n1, CExpInteger n2) ->
      (CExpInteger (n1 `quot` (max 1 n2)), s'')
    --peano
    _ -> do
      let n1 = peanoToInteger e1'
      let n2 = peanoToInteger e2'
      (integerToPeano (n1 `quot` (max 1 n2)), s'')

modEval :: CExp -> CExp -> Store -> (CExp, Store)
modEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case (e1', e2') of
    --Nonpeano
    (CExpInteger n1, CExpInteger n2) ->
      (CExpInteger (n1 `rem` (max 1 n2)), s'')
    --peano
    _ -> do
      let n1 = peanoToInteger e1'
      let n2 = peanoToInteger e2'
      (integerToPeano (n1 `rem` (max 1 n2)), s'')

powerEval :: CExp -> CExp -> Store -> (CExp, Store)
powerEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case (e1', e2') of
    --Nonpeano
    (CExpInteger n1, CExpInteger n2) ->
      (CExpInteger (n1 ^ n2), s'')
    --peano
    _ -> do
      let n1 = peanoToInteger e1'
      let n2 = peanoToInteger e2'
      (integerToPeano (n1 ^ n2), s'')

peanoToInteger :: CExp -> Integer
peanoToInteger e =
  case e of
    CExpZero -> 0
    CExpSuc x -> 1 + (peanoToInteger x)
    _ -> error "Should not happen. peanoToInteger applied to a non-peano expression"

integerToPeano :: Integer -> CExp
integerToPeano n =
  if n == 0 then CExpZero else CExpSuc (integerToPeano (n -1))

listEval :: [CExp] -> Store -> (CExp, Store)
listEval ceList s = do
  let (ceList', s') = helper ceList s
  (CExpList ceList', s')
  where
    helper :: [CExp] -> Store -> ([CExp], Store)
    helper xs store =
      case xs of
        [] -> ([], store)
        x : xs' -> do
          let (x', store') = evaluationInterpreter x store
          let (xs'', store'') = helper xs' store'
          (x' : xs'', store'')

listAppendEval :: CExp -> CExp -> Store -> (CExp, Store)
listAppendEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case (e1', e2') of
    (CExpList lis1, CExpList lis2) ->
      (CExpList (lis1 ++ lis2), s'')
    (CExpList lis1, _) -> error "Should not happen. listAppendEval applied to a non-list second expression"
    _ -> error "Should not happen. listAppendEval applied to a non-list first expression"

listConsEval :: CExp -> CExp -> Store -> (CExp, Store)
listConsEval e1 e2 s = do
  let (e1', s') = evaluationInterpreter e1 s
  let (e2', s'') = evaluationInterpreter e2 s'
  case e2' of
    CExpList lis2 ->
      (CExpList (e1' : lis2), s'')
    _ -> error "Should not happen. listConsEval applied to a non-list second expression"

listRecEval :: CExp -> CExp -> CExp -> Store -> (CExp, Store)
listRecEval e e1 e2 s =
  do
    let (e', s') = evaluationInterpreter e s
    case e' of
      CExpList eList ->
        case eList of
          [] -> evaluationInterpreter e1 s'
          x : xs -> do
            let nextExp = CExpListRec (CExpList xs) e1 e2
            let (nextValue, s'') = evaluationInterpreter nextExp s'
            let e2' = substituteCExp nextValue 0 e2
            let e2'' = substituteCExp (CExpList xs) 1 e2'
            let finalExp = substituteCExp x 2 e2''
            evaluationInterpreter finalExp s''
      _ -> error "Should not happen! listRecEval applied to a until expression"

{-
import Datatype
import Interpreter.ExpFunctions
import Interpreter.StoreFunctions
import Interpreter.ValueChecker

evaluationInterpreter :: AExp -> Store -> (AExp, Store)
evaluationInterpreter exp s =
  if isValue exp
    then (exp, s)
    else case exp of
      AExpVar str -> error ("Should not happen! evaluationInterpreter called on a AExpVar " ++ str)
      AExpUnit -> error ("Should not happen! isValue did not detect Unit as value")
      AExpLambda str at ae -> error ("Should not happen! isValue did not detect Lambda abstraction as value")
      AExpApplication ae ae' -> aExpApplicationEval ae ae' s
      AExpProduct ae ae' -> aExpProductEval ae ae' s
      AExpFst ae -> aExpFstEval ae s
      AExpSnd ae -> aExpSndEval ae s
      AExpInl ae at -> aExpInlEval ae at s
      AExpInr ae at -> aExpInrEval ae at s
      AExpMatch ae str ae' cs ae2 -> aExpMatchEval ae str ae' cs ae2 s
      AExpZero -> error ("Should not happen! isValue did not detect Zero as value")
      AExpSuc ae -> aExpSucEval ae s
      AExpPrimrec ae ae' str cs ae2 -> aExpPrimrecEval ae ae' str cs ae2 s
      AExpArrow ae -> aExpArrowEval ae s
      AExpAt ae -> aExpAtEval ae s
      AExpAdv ae -> aExpAdvEval ae s
      AExpBox ae -> error ("Should not happen! isValue did not detect Box as value")
      AExpUnbox ae -> aExpUnboxEval ae s
      AExpNow ae at -> aExpNowEval ae at s
      AExpWait ae ae' -> aExpWaitEval ae ae' s
      AExpUrec ae str ae' cs s' str' ae2 -> aExpUrecEval ae str ae' cs s' str' ae2 s
      AExpFix str at ae -> error ("Should not happen! isValue did not detect fix as value")
      AExpOut ae -> aExpOutEval ae s
      AExpInto ae at -> aExpIntoEval ae at s
      AExpLocation n -> error ("Should not happen! isValue did not detect location as value")

aExpApplicationEval :: AExp -> AExp -> Store -> (AExp, Store)
aExpApplicationEval ae ae' s = do
  let (exp', s') = evaluationInterpreter ae s
  case exp' of
    AExpLambda var _ lambdaexp ->
      do
        let (v, s'') = evaluationInterpreter ae' s'
        evaluationInterpreter (substituteExp lambdaexp var v) s''
    _ -> error ("Should not happen! exp does not evaluate to lambda abstraction value")

aExpProductEval :: AExp -> AExp -> Store -> (AExp, Store)
aExpProductEval t t' s =
  do
    let (v, s') = evaluationInterpreter t s
    let (v', s'') = evaluationInterpreter t' s'
    (AExpProduct v v', s'')

aExpFstEval :: AExp -> Store -> (AExp, Store)
aExpFstEval t s =
  do
    let (v, s') = evaluationInterpreter t s
    case v of
      AExpProduct v1 v2 -> (v1, s')
      _ -> error "Should not happen! fst not applied to a product"

aExpSndEval :: AExp -> Store -> (AExp, Store)
aExpSndEval t s =
  do
    let (v, s') = evaluationInterpreter t s
    case v of
      AExpProduct v1 v2 -> (v2, s')
      _ -> error "Should not happen! snd not applied to a product"

aExpInlEval :: AExp -> AType -> Store -> (AExp, Store)
aExpInlEval t tascrip s =
  do
    let (v, s') = evaluationInterpreter t s
    (AExpInl v tascrip, s')

aExpInrEval :: AExp -> AType -> Store -> (AExp, Store)
aExpInrEval t tascrip s =
  do
    let (v, s') = evaluationInterpreter t s
    (AExpInr v tascrip, s')

aExpMatchEval :: AExp -> String -> AExp -> String -> AExp -> Store -> (AExp, Store)
aExpMatchEval t x t1 y t2 s =
  do
    let (sumv, s') = evaluationInterpreter t s
    case sumv of
      AExpInl v tascrip ->
        evaluationInterpreter (substituteExp t1 x v) s'
      AExpInr v tascrip ->
        evaluationInterpreter (substituteExp t2 y v) s'
      _ -> error "Should not happen! match not applied to a sum"

aExpSucEval :: AExp -> Store -> (AExp, Store)
aExpSucEval t s =
  do
    let (v, s') = evaluationInterpreter t s
    (AExpSuc v, s')

aExpPrimrecEval :: AExp -> AExp -> String -> String -> AExp -> Store -> (AExp, Store)
aExpPrimrecEval n exp1 x y exp2 s =
  do
    let (num, s') = evaluationInterpreter n s
    case num of
      AExpZero -> evaluationInterpreter exp1 s'
      AExpSuc v ->
        do
          let (v', s'') = aExpPrimrecEval v exp1 x y exp2 s'
          evaluationInterpreter (substituteExp (substituteExp exp2 x v) y v') s''
      _ -> error "Should not happen! primrec not applied to a natural"

aExpArrowEval :: AExp -> Store -> (AExp, Store)
aExpArrowEval t s =
  case s of
    NullStore -> error "Should not happen! Arrow called with null store"
    other -> do
      let (s', l) = addStoreElem s t
      (l, s')

aExpAtEval :: AExp -> Store -> (AExp, Store)
aExpAtEval t s =
  case s of
    NullStore -> error "Should not happen! Arrow called with null store"
    other -> do
      let (s', l) = addStoreElem s t
      (l, s')

aExpAdvEval :: AExp -> Store -> (AExp, Store)
aExpAdvEval t s =
  case s of
    TickStore etaN etaL ->
      do
        let (lexp, s') = evaluationInterpreter t (TicklessStore etaN)
        case (lexp, s') of
          (AExpLocation l, TicklessStore etaN') ->
            do
              let exp = elemStore etaN' l
              case exp of
                Nothing -> error " Should not happen! Cant find location in adv."
                Just ae -> evaluationInterpreter (ae) (TickStore etaN' etaL)
          _ -> error "Should not happen! t in adv t does not produce location with tickless store."
    _ -> error "Should not happen! adv called with non TickStore"

aExpUnboxEval :: AExp -> Store -> (AExp, Store)
aExpUnboxEval t s =
  case s of
    NullStore -> error "Should not happen! unbox done with nullstore!"
    _ ->
      do
        let (result, expectedNullStore) = evaluationInterpreter t NullStore
        case expectedNullStore of
          NullStore ->
            case result of
              AExpBox t' -> evaluationInterpreter t' s
              AExpFix x tascrip t' ->
                do
                  let subExp = AExpBox (AExpArrow (AExpUnbox result))
                  evaluationInterpreter (substituteExp t' x (subExp)) s
              _ -> error "Should not happen! Unboxing an expression that is not a box or fix expression"
          _ -> error "REALLY should not happen! Nullstore evaluated to a non-nullstore in unbox"

aExpNowEval :: AExp -> AType -> Store -> (AExp, Store)
aExpNowEval t tascrip s =
  do
    let (v, s') = evaluationInterpreter t s
    (AExpNow v tascrip, s')

aExpWaitEval :: AExp -> AExp -> Store -> (AExp, Store)
aExpWaitEval t1 t2 s =
  do
    let (v1, s') = evaluationInterpreter t1 s
    let (v2, s'') = evaluationInterpreter t2 s'
    (AExpWait v1 v2, s'')

aExpUrecEval :: AExp -> String -> AExp -> [Char] -> [Char] -> [Char] -> AExp -> Store -> (AExp, Store)
aExpUrecEval u var1 exp1 x y z exp2 s =
  do
    let (result, s') = evaluationInterpreter u s
    case result of
      AExpNow v tascrip ->
        evaluationInterpreter (substituteExp exp1 var1 v) s'
      AExpWait v1 v2 ->
        do
          let storeExp = AExpUrec (AExpAdv v2) var1 exp1 x y z exp2
          let (newStore, location) = addStoreElem s' storeExp
          let newExp2 = substituteExp (substituteExp (substituteExp exp2 x v1) y v2) z location
          evaluationInterpreter newExp2 newStore
      _ -> error "Should not happen! urec applied to a non now/wait expression!"

aExpOutEval :: AExp -> Store -> (AExp, Store)
aExpOutEval t s =
  do
    let (result, s') = evaluationInterpreter t s
    case result of
      AExpInto v tascrip ->
        (v, s')
      _ -> error "Should not happen! out applied to a non-into expression"

aExpIntoEval :: AExp -> AType -> Store -> (AExp, Store)
aExpIntoEval t tascrip s =
  do
    let (v, s') = evaluationInterpreter t s
    (AExpInto v tascrip, s')

    -}