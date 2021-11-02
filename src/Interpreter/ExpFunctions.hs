--Module to be modified

module Interpreter.ExpFunctions where

{-
import Datatype

substituteExp :: AExp -> String -> AExp -> AExp
substituteExp exp s exp' = case exp of
  AExpVar str -> if s == str then exp' else AExpVar str
  AExpUnit -> AExpUnit
  AExpLambda str at ae -> if s == str then AExpLambda str at ae else AExpLambda str at (substituteExp ae s exp')
  AExpApplication ae ae' -> AExpApplication (substituteExp ae s exp') (substituteExp ae' s exp')
  AExpProduct ae ae' -> AExpProduct (substituteExp ae s exp') (substituteExp ae' s exp')
  AExpFst ae -> AExpFst (substituteExp ae s exp')
  AExpSnd ae -> AExpSnd (substituteExp ae s exp')
  AExpInl ae at -> AExpInl (substituteExp ae s exp') at
  AExpInr ae at -> AExpInr (substituteExp ae s exp') at
  AExpMatch ae str ae' cs ae2 ->
    AExpMatch
      (substituteExp ae s exp')
      str
      (if s == str then ae' else (substituteExp ae' s exp'))
      cs
      (if s == cs then ae2 else (substituteExp ae2 s exp'))
  AExpZero -> AExpZero
  AExpSuc ae -> AExpSuc (substituteExp ae s exp')
  AExpPrimrec ae ae' str cs ae2 ->
    AExpPrimrec
      (substituteExp ae s exp')
      (substituteExp ae' s exp')
      str
      cs
      (if s == str || s == cs then ae2 else (substituteExp ae2 s exp'))
  AExpArrow ae -> AExpArrow (substituteExp ae s exp')
  AExpAt ae -> AExpAt (substituteExp ae s exp')
  AExpAdv ae -> AExpAdv (substituteExp ae s exp')
  AExpBox ae -> AExpBox (substituteExp ae s exp')
  AExpUnbox ae -> AExpUnbox (substituteExp ae s exp')
  AExpNow ae at -> AExpNow (substituteExp ae s exp') at
  AExpWait ae ae' -> AExpWait (substituteExp ae s exp') (substituteExp ae' s exp')
  AExpUrec ae str ae' cs s' str' ae2 ->
    AExpUrec
      (substituteExp ae s exp')
      str
      (if s == str then ae' else (substituteExp ae' s exp'))
      cs
      s'
      str'
      (if s == cs || s == s' || s == str' then ae2 else (substituteExp ae2 s exp'))
  AExpFix str at ae -> AExpFix str at (if s == str then ae else (substituteExp ae s exp'))
  AExpOut ae -> AExpOut (substituteExp ae s exp')
  AExpInto ae at -> AExpInto (substituteExp ae s exp') at
  AExpLocation n -> AExpLocation n

  -}