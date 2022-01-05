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
  try importStatementParser
    <|> try typeStatementParser
    <|> try defStatementParser

defStatementParser :: Parser Statement
defStatementParser = do
  skipMany space
  string "def"
  skipMany1 space
  var <- varParser
  spaces
  polyParameters <- optionMaybe (try parameterParser)
  spaces
  firstParameters <- many (annoVarParser)
  spaces
  pound <- optionMaybe (try (char '#'))
  spaces
  secondParameters <- many (annoVarParser)
  spaces
  char '='
  notFollowedBy (char '>')
  spaces
  exp <- expParser
  spaces
  -- char ';'
  -- spaces
  let modifiedexp = modifyExp firstParameters pound secondParameters exp
  case polyParameters of
    Nothing -> return (DefStatement var [] modifiedexp)
    Just ss -> return (DefStatement var ss modifiedexp)

parameterParser :: Parser [(TypeProperty, String)]
parameterParser =
  do
    char '{'
    spaces
    l <- sepBy1 oneParameterParser (try commaParser)
    spaces
    char '}'
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
          string "Comparable"
          skipMany1 space
          v <- varParser
          return (Comparable, v)
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
          return (LimitStable, v)
      )
    <|> try
      ( do
          string "Stable"
          skipMany1 space
          string "Limit"
          skipMany1 space
          v <- varParser
          return (LimitStable, v)
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
  notFollowedBy (char '>')
  spaces
  t <- typeParser
  spaces
  --char ';'
  --spaces
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
  alias <- optionMaybe aliasParser
  --char ';'
  spaces
  let processedFilePath = (processFilePath s) ++ ".eva"
  return (ImportStatement processedFilePath alias)

aliasParser :: Parser String
aliasParser =
  try
    ( do
        string "as"
        skipMany1 space
        alias <- upperVarParser
        checkUpperVar alias
    )

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
  spaces
  eof
  return p

mainParser :: FilePath -> String -> Either ParseError Program
mainParser file = parse programParser ("Unable to parse file: " ++ file)
