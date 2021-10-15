module Interpreter.EvaluationInterpreter where

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