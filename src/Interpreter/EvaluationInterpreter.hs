module Interpreter.EvaluationInterpreter where

import Datatype
import Interpreter.ExpFunctions
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
      AExpFst ae -> error ("TODO")
      AExpSnd ae -> error ("TODO")
      AExpInl ae at -> error ("TODO")
      AExpInr ae at -> error ("TODO")
      AExpMatch ae str ae' cs ae2 -> error ("TODO")
      AExpZero -> error ("TODO")
      AExpSuc ae -> error ("TODO")
      AExpPrimrec ae ae' str cs ae2 -> error ("TODO")
      AExpArrow ae -> error ("TODO")
      AExpAt ae -> error ("TODO")
      AExpAdv ae -> error ("TODO")
      AExpBox ae -> error ("TODO")
      AExpUnbox ae -> error ("TODO")
      AExpNow ae at -> error ("TODO")
      AExpWait ae ae' -> error ("TODO")
      AExpUrec ae str ae' cs s' str' ae2 -> error ("TODO")
      AExpFix str at ae -> error ("TODO")
      AExpOut ae -> error ("TODO")
      AExpInto ae at -> error ("TODO")
      AExpLocation n -> error ("TODO")

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