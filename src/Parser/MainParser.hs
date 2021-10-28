module Parser.MainParser where

import Datatype
import Parser.ExpParser
import Parser.TypeParser
import Parser.VarParser
import Text.Parsec
import Text.Parsec.Combinator (eof)
import Text.Parsec.String

statementParser :: Parser Statement
statementParser =
  try letStatementParser
    <|> try typeStatementParser
    <|> importStatementParser

letStatementParser :: Parser Statement
letStatementParser = do
  skipMany space
  string "let"
  skipMany1 space
  var <- varParser
  spaces
  char '='
  spaces
  exp <- expParser
  spaces
  char ';'
  spaces
  return (LetStatement var exp)

typeStatementParser :: Parser Statement
typeStatementParser = do
  spaces
  string "type"
  skipMany1 space
  -- some code
  spaces
  char ';'
  spaces
  error "type statements not supported yet"

importStatementParser :: Parser Statement
importStatementParser = do
  spaces
  string "import"
  skipMany1 space
  -- some code
  spaces
  char ';'
  spaces
  error "import statements not supported yet"

programParser :: Parser Program
programParser = do
  p <- many statementParser
  eof
  return p

mainParser :: String -> Either ParseError Program
mainParser = parse programParser "Unable to parse file"
