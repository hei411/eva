module Parser where

import Datatype
import Text.Parsec
import Text.Parsec.String

whiteSpaceCharacters :: [Char]
whiteSpaceCharacters = " \n\r "

whiteSpacesOptionalParser :: Parser String
whiteSpacesOptionalParser = many (oneOf whiteSpaceCharacters)

whiteSpacesSomeParser :: Parser String
whiteSpacesSomeParser = many1 (oneOf whiteSpaceCharacters)

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