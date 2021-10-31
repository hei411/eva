module Parser.ExpParser where

import Datatype
import Parser.TypeParser
import Parser.VarParser
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Combinator (notFollowedBy)

expParser :: Parser AExp
expParser =
  try bindExpParser
    <|> try
      ( do
          first <- appExpParser
          spaces
          second <- bindExpParser
          return (AExpApplication first second)
      )
    <|> appExpParser
    <|> fail "Cannot parse an expression"

lambdaParser :: Parser AExp
lambdaParser = do
  string "fun"
  notFollowedBy alphaNum
  spaces
  v <- varParser
  spaces
  char ':'
  spaces
  t <- typeParser
  spaces
  string "=>"
  spaces
  exp <- expParser
  return (AExpLambda v t exp)

fixParser :: Parser AExp
fixParser = do
  string "fix"
  skipMany1 space
  v <- varParser
  spaces
  char ':'
  spaces
  t <- typeParser
  spaces
  string "=>"
  spaces
  exp <- expParser
  return (AExpFix v t exp)

bindExpParser :: Parser AExp
bindExpParser =
  try lambdaParser
    <|> fixParser
    <|> fail "Can't parse lambda abstraction or fix abstraction"

appExpParser :: Parser AExp
appExpParser = do
  h <- oneExpParser
  t <-
    many
      ( try
          ( do
              spaces
              oneExpParser
          )
      )
  return (foldApplication (h : t))

foldApplication :: [AExp] -> AExp
foldApplication l = case l of
  [] -> error "Should not happen!"
  h : t -> foldApplicationHelper h t
  where
    foldApplicationHelper :: AExp -> [AExp] -> AExp
    foldApplicationHelper h t = case t of
      [] -> h
      hd : tl -> foldApplicationHelper (AExpApplication h hd) tl

firstExpParser :: Parser AExp
firstExpParser =
  try bindExpParser
    <|> oneExpParser
    <|> fail "Can't parse FirstExp"

parenthesisParser :: Parser AExp
parenthesisParser = do
  char '('
  spaces
  exp <- expParser
  spaces
  char ')'
  return exp

productParser :: Parser AExp
productParser = do
  char '('
  spaces
  first <- expParser
  spaces
  char ','
  second <- expParser
  spaces
  char ')'
  return (AExpProduct first second)

fstParser :: Parser AExp
fstParser = do
  string "fst"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpFst exp)

sndParser :: Parser AExp
sndParser = do
  string "snd"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpSnd exp)

inlParser :: Parser AExp
inlParser = do
  string "inl"
  notFollowedBy alphaNum
  spaces
  exp <- expParser
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpInl exp t)

inrParser :: Parser AExp
inrParser = do
  string "inr"
  notFollowedBy alphaNum
  spaces
  exp <- expParser
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpInr exp t)

matchParser :: Parser AExp
matchParser = do
  string "match"
  skipMany1 space
  exp <- expParser
  spaces
  string "with"
  spaces
  char '|'
  spaces
  string "inl"
  skipMany1 space
  v1 <- varParser
  spaces
  string "=>"
  spaces
  exp1 <- expParser
  spaces
  char '|'
  spaces
  string "inr"
  skipMany1 space
  v2 <- varParser
  spaces
  string "=>"
  spaces
  exp2 <- expParser
  return (AExpMatch exp v1 exp1 v2 exp2)

numberParser :: Parser AExp
numberParser = do
  n <- many1 digit
  notFollowedBy letter
  let num = read n :: Integer
  return (wrapSuc num)

wrapSuc :: Integer -> AExp
wrapSuc n =
  case n of
    0 -> AExpZero
    _ -> AExpSuc (wrapSuc (n -1))

sucParser :: Parser AExp
sucParser = do
  string "suc"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpSuc exp)

primrecParser :: Parser AExp
primrecParser = do
  string "primrec"
  notFollowedBy alphaNum
  spaces
  exp <- expParser
  spaces
  string "with"
  spaces
  char '|'
  spaces
  char '0'
  spaces
  string "=>"
  spaces
  exp1 <- expParser
  spaces
  char '|'
  spaces
  string "suc"
  skipMany1 space
  v1 <- varParser
  skipMany1 space
  string "fby"
  skipMany1 space
  v2 <- varParser
  spaces
  string "=>"
  spaces
  exp2 <- expParser
  return (AExpPrimrec exp exp1 v1 v2 exp2)

arrowParser :: Parser AExp
arrowParser = do
  char '>'
  spaces
  exp <- firstExpParser
  return (AExpArrow exp)

atParser :: Parser AExp
atParser = do
  char '@'
  spaces
  exp <- firstExpParser
  return (AExpAt exp)

advParser :: Parser AExp
advParser = do
  string "adv"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpAdv exp)

unboxParser :: Parser AExp
unboxParser = do
  string "unbox"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpUnbox exp)

boxParser :: Parser AExp
boxParser = do
  string "[]"
  spaces
  exp <- firstExpParser
  return (AExpBox exp)

nowParser :: Parser AExp
nowParser = do
  string "now"
  notFollowedBy alphaNum
  spaces
  exp <- expParser
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpNow exp t)

waitParser :: Parser AExp
waitParser = do
  string "wait"
  notFollowedBy alphaNum
  spaces
  exp1 <- oneExpParser
  spaces
  exp2 <- firstExpParser
  return (AExpWait exp1 exp2)

urecParser :: Parser AExp
urecParser = do
  string "urec"
  notFollowedBy alphaNum
  spaces
  exp <- expParser
  spaces
  string "with"
  spaces
  char '|'
  spaces
  string "now"
  skipMany1 space
  v1 <- varParser
  spaces
  string "=>"
  spaces
  exp1 <- expParser
  spaces
  char '|'
  spaces
  string "wait"
  skipMany1 space
  v2 <- varParser
  skipMany1 space
  v3 <- varParser
  skipMany1 space
  string "fby"
  skipMany1 space
  v4 <- varParser
  spaces
  string "=>"
  spaces
  exp2 <- expParser
  return (AExpUrec exp v1 exp1 v2 v3 v4 exp2)

outParser :: Parser AExp
outParser = do
  string "out"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpOut exp)

intoParser :: Parser AExp
intoParser = do
  string "into"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpInto exp t)

expVarParser :: Parser AExp
expVarParser = do
  str <- varParser
  parameters <- optionMaybe (try expParameterParser)
  case parameters of
    Nothing -> return (AExpVar str [])
    Just ats -> return (AExpVar str ats)

expParameterParser :: Parser [AType]
expParameterParser =
  do
    spaces
    char '<'
    spaces
    l <- sepBy1 typeParser (try commaParser)
    spaces
    char '>'
    return l
  where
    commaParser :: Parser ()
    commaParser =
      do
        spaces
        char ','
        spaces

unitParser :: Parser AExp
unitParser = do
  char '('
  spaces
  char ')'
  return AExpUnit

oneExpParser :: Parser AExp
oneExpParser =
  try parenthesisParser
    <|> try productParser
    <|> try fstParser
    <|> try sndParser
    <|> try inlParser
    <|> try inrParser
    <|> try matchParser
    <|> try numberParser
    <|> try sucParser
    <|> try primrecParser
    <|> try arrowParser
    <|> try atParser
    <|> try advParser
    <|> try unboxParser
    <|> try boxParser
    <|> try nowParser
    <|> try waitParser
    <|> try urecParser
    <|> try outParser
    <|> try intoParser
    <|> try expVarParser
    <|> unitParser
    <|> fail "Can't parse OneExp"
