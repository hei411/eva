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
  parameters <- optionMaybe (try parameterParser)
  spaces
  -- TODO: syntactic sugar for functions
  spaces
  char '='
  spaces
  exp <- expParser
  spaces
  char ';'
  spaces
  case parameters of
    Nothing -> return (LetStatement var [] exp)
    Just ss -> return (LetStatement var ss exp)

parameterParser :: Parser [(TypeProperty, String)]
parameterParser =
  do
    char '<'
    spaces
    l <- sepBy1 oneParameterParser (try commaParser)
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

oneParameterParser :: Parser (TypeProperty, String)
oneParameterParser =
  try
    ( do
        v <- varParser
        return (None, v)
    )
    <|> try
      ( do
          string "Stable"
          skipMany1 space
          v <- varParser
          return (Stable, v)
      )
    <|> try
      ( do
          string "Limit"
          skipMany1 space
          v <- varParser
          return (Limit, v)
      )
    <|> try
      ( do
          string "Limit"
          skipMany1 space
          string "Stable"
          skipMany1 space
          v <- varParser
          return (Both, v)
      )
    <|> try
      ( do
          string "Stable"
          skipMany1 space
          string "Limit"
          skipMany1 space
          v <- varParser
          return (Both, v)
      )

typeStatementParser :: Parser Statement
typeStatementParser = do
  spaces
  string "type"
  skipMany1 space
  str <- upperVarParser
  spaces
  parameters <- optionMaybe (try typenameParameterParser)
  spaces
  char '='
  spaces
  t <- typeParser
  spaces
  char ';'
  spaces
  case parameters of
    Nothing -> return (TypeStatement str [] t)
    Just ss -> return (TypeStatement str ss t)

typenameParameterParser :: Parser [String]
typenameParameterParser = do
  char '('
  spaces
  l <- sepBy1 varParser (try commaParser)
  spaces
  char ')'
  return l
  where
    commaParser :: Parser ()
    commaParser =
      do
        spaces
        char ','
        spaces

importStatementParser :: Parser Statement
importStatementParser = do
  spaces
  string "import"
  skipMany1 space
  s <- fileNameParser
  spaces
  char ';'
  spaces
  let processedFilePath = processFilePath s
  return (ImportStatement processedFilePath)

processFilePath :: FilePath -> FilePath
processFilePath s =
  case s of
    [] -> error "Did not provide file path!"
    _ ->
      replace_separator s
  where
    replace_separator s =
      case s of
        [] -> []
        hd : tl -> if hd == '.' then '/' : replace_separator tl else hd : replace_separator tl

fileNameParser :: Parser FilePath
fileNameParser =
  many (choice [alphaNum, oneOf "-_."])

programParser :: Parser Program
programParser = do
  p <- many statementParser
  eof
  return p

mainParser :: String -> Either ParseError Program
mainParser = parse programParser "Unable to parse file"
