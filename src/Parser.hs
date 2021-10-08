module Parser where

import Datatype
import Text.Parsec
import Text.Parsec.String

whiteSpacesSomeParser :: Parser ()
whiteSpacesSomeParser = skipMany1 space

unitParser :: Parser AExp
unitParser = do
  string "()"
  return AExpUnit

zeroParser :: Parser AExp
zeroParser = do
  char '0'
  return AExpZero

expVarParser :: Parser AExp
expVarParser = do
  start <- lower
  rest <- many alphaNum
  let str = start : rest
  case str of
    "fst" -> fail "fst cannot be variable name."
    "snd" -> fail "fst cannot be variable name."
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

mainParser :: String -> Either ParseError Char
mainParser = parse (char '\n') "Unable to parse file"