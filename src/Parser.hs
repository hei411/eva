module Parser where

import Datatype
import Text.Parsec
import Text.Parsec.String

statementParser :: Parser Statement
statementParser = letStatementParser

expParser :: Parser AExp
expParser =
  try bindExpParser
    <|> try
      ( do
          first <- appExpParser
          skipMany1 space
          second <- bindExpParser
          return (AExpApplication first second)
      )
    <|> appExpParser
    <|> fail "Cannot parse an expression"

lambdaParser :: Parser AExp
lambdaParser = do
  char '\\'
  spaces
  AExpVar v <- expVarParser
  spaces
  char ':'
  t <- typeParser
  spaces
  string "->"
  spaces
  exp <- expParser
  return (AExpLambda v t exp)

fixParser :: Parser AExp
fixParser = do
  string "fix"
  skipMany1 space
  AExpVar v <- expVarParser
  spaces
  char ':'
  t <- typeParser
  spaces
  string "->"
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
  l <- many1 oneExpParser
  return (foldApplication l)

foldApplication :: [AExp] -> AExp
foldApplication l = case l of
  [] -> error "Should not happen!"
  h : t -> foldApplicationHelper h t
  where
    foldApplicationHelper :: AExp -> [AExp] -> AExp
    foldApplicationHelper h t = case t of
      [] -> h
      hd : tl -> foldApplicationHelper (AExpApplication h hd) t

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
  skipMany1 space
  exp <- firstExpParser
  return (AExpFst exp)

sndParser :: Parser AExp
sndParser = do
  string "snd"
  skipMany1 space
  exp <- firstExpParser
  return (AExpSnd exp)

inlParser :: Parser AExp
inlParser = do
  string "inl"
  skipMany1 space
  exp <- expParser
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpInl exp t)

inrParser :: Parser AExp
inrParser = do
  string "inr"
  skipMany1 space
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
  skipMany1 space
  string "with"
  spaces
  char '|'
  spaces
  string "inl"
  skipMany1 space
  AExpVar v1 <- expVarParser
  spaces
  string "->"
  spaces
  exp1 <- expParser
  spaces
  char '|'
  spaces
  string "inr"
  skipMany1 space
  AExpVar v2 <- expVarParser
  spaces
  string "->"
  spaces
  exp2 <- expParser
  return (AExpMatch exp v1 exp1 v2 exp2)

zeroParser :: Parser AExp
zeroParser = do
  char '0'
  return AExpZero

sucParser :: Parser AExp
sucParser = do
  string "suc"
  skipMany1 space
  exp <- firstExpParser
  return (AExpSuc exp)

primrecParser :: Parser AExp
primrecParser = do
  string "primrec"
  skipMany1 space
  exp <- expParser
  skipMany1 space
  string "with"
  spaces
  char '|'
  spaces
  char '0'
  spaces
  string "->"
  spaces
  exp1 <- expParser
  spaces
  char '|'
  spaces
  string "suc"
  skipMany1 space
  AExpVar v1 <- expVarParser
  skipMany1 space
  string "fby"
  skipMany1 space
  AExpVar v2 <- expVarParser
  spaces
  string "->"
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
  skipMany1 space
  exp <- firstExpParser
  return (AExpAdv exp)

unboxParser :: Parser AExp
unboxParser = do
  string "unbox"
  skipMany1 space
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
  skipMany1 space
  exp <- expParser
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpNow exp t)

waitParser :: Parser AExp
waitParser = do
  string "wait"
  skipMany1 space
  exp1 <- oneExpParser
  skipMany1 space
  exp2 <- firstExpParser
  return (AExpWait exp1 exp2)

urecParser :: Parser AExp
urecParser = do
  string "urec"
  skipMany1 space
  exp <- expParser
  skipMany1 space
  string "with"
  spaces
  char '|'
  spaces
  string "now"
  skipMany1 space
  AExpVar v1 <- expVarParser
  spaces
  string "->"
  spaces
  exp1 <- expParser
  spaces
  char '|'
  spaces
  string "wait"
  skipMany1 space
  AExpVar v2 <- expVarParser
  skipMany1 space
  AExpVar v3 <- expVarParser
  skipMany1 space
  string "fby"
  skipMany1 space
  AExpVar v4 <- expVarParser
  spaces
  string "->"
  spaces
  exp2 <- expParser
  return (AExpUrec exp v1 exp1 v2 v3 v4 exp2)

outParser :: Parser AExp
outParser = do
  string "out"
  skipMany1 space
  exp <- firstExpParser
  return (AExpOut exp)

intoParser :: Parser AExp
intoParser = do
  string "into"
  skipMany1 space
  exp <- firstExpParser
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpInto exp t)

expVarParser :: Parser AExp
expVarParser = do
  start <- lower
  rest <- many alphaNum
  let str = start : rest
  case str of
    "fst" -> fail "fst cannot be variable name."
    "snd" -> fail "snd cannot be variable name."
    "inl" -> fail "inl cannot be variable name."
    "inr" -> fail "inr cannot be variable name."
    "suc" -> fail "suc cannot be variable name."
    "match" -> fail "match cannot be variable name."
    "with" -> fail "with cannot be variable name."
    "primrec" -> fail "primrec cannot be variable name."
    "let" -> fail "let cannot be variable name."
    "type" -> fail "type cannot be variable name."
    "fby" -> fail "fby cannot be variable name."
    "adv" -> fail "adv cannot be variable name."
    "unbox" -> fail "unbox cannot be variable name."
    "now" -> fail "now cannot be variable name."
    "urec" -> fail "urec cannot be variable name."
    "fix" -> fail "fix cannot be variable name."
    "into" -> fail "into cannot be variable name."
    "out" -> fail "out cannot be variable name."
    _ -> return (AExpVar (start : rest))

unitParser :: Parser AExp
unitParser = do
  string "()"
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
    <|> try zeroParser
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

typeParser :: Parser AType
typeParser = do
  -- TODO types parser
  return ATypeUnit

letStatementParser :: Parser Statement
letStatementParser = do
  skipMany space
  string "let"
  skipMany1 space
  AExpVar var <- expVarParser
  spaces
  char '='
  spaces
  exp <- expParser
  spaces
  char ';'
  spaces
  return (LetStatement var exp)

programParser :: Parser Program
programParser = many statementParser

mainParser :: String -> Either ParseError Program
mainParser = parse programParser "Unable to parse file"