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

mainParser :: String -> Either ParseError Char
mainParser = parse (char '\n') "Unable to parse file"