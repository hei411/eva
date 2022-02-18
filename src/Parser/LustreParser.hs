module Parser.LustreParser where

import Datatype
import Text.Parsec
import Text.Parsec.String

lustreStatementParser :: Parser Statement
lustreStatementParser =
  do
    spaces
    string "{-"
    spaces
    string "node"
    skipMany1 space
    nodeName <- lustreVarParser
    spaces
    args <- lustreArgsParser
    spaces
    string "returns"
    spaces
    returnVar <- lustreReturnVarParser
    spaces
    char ';'
    spaces
    string "let"
    skipMany1 space
    definition <- lustreDefParser
    spaces
    string "tel"
    spaces
    string "-}"
    spaces
    return (LustreStatement nodeName args returnVar definition)

lustreDefParser :: Parser LustreDef
lustreDefParser =
  do
    v <- lustreVarParser
    spaces
    char '='
    spaces
    exp <- lustreExpParser
    spaces
    exp2 <- optionMaybe (try arrowExpParser)
    spaces
    char ';'
    case exp2 of
      Nothing -> return (LustreSimpleExp v exp)
      Just le -> return (LustreArrowExp v exp le)
  where
    arrowExpParser :: Parser LustreExp
    arrowExpParser =
      do
        string "->"
        spaces
        e <- lustreExpParser
        return e

lustreExpParser :: Parser LustreExp
lustreExpParser =
  do
    chainl1 orLustreExpParser operator
  where
    operator :: Parser (LustreExp -> LustreExp -> LustreExp)
    operator =
      try
        ( do
            spaces
            string "="
            spaces
            return LustreExpEquals
        )
        <|> try
          ( do
              spaces
              string "<"
              spaces
              return LustreExpSmaller
          )
        <|> try
          ( do
              spaces
              string ">"
              spaces
              return LustreExpLarger
          )

orLustreExpParser :: Parser LustreExp
orLustreExpParser =
  do
    chainl1 andLustreExpParser operator
  where
    operator :: Parser (LustreExp -> LustreExp -> LustreExp)
    operator =
      try
        ( do
            spaces
            string "or"
            notFollowedBy alphaNum
            spaces
            return LustreExpOr
        )

andLustreExpParser :: Parser LustreExp
andLustreExpParser =
  do
    chainl1 notLustreExpParser operator
  where
    operator :: Parser (LustreExp -> LustreExp -> LustreExp)
    operator =
      try
        ( do
            spaces
            string "and"
            notFollowedBy alphaNum
            spaces
            return LustreExpAnd
        )

notLustreExpParser :: Parser LustreExp
notLustreExpParser =
  try
    ( do
        string "not"
        notFollowedBy alphaNum
        spaces
        e <- notLustreExpParser
        return (LustreExpNot e)
    )
    <|> addSubtractLustreExpParser

addSubtractLustreExpParser :: Parser LustreExp
addSubtractLustreExpParser =
  do
    chainl1 multiplyDivideLustreExpParser operator
  where
    operator :: Parser (LustreExp -> LustreExp -> LustreExp)
    operator =
      try
        ( do
            spaces
            string "+"
            spaces
            return LustreExpAdd
        )
        <|> try
          ( do
              spaces
              string "-"
              notFollowedBy (char '>')
              spaces
              return LustreExpMinus
          )

multiplyDivideLustreExpParser :: Parser LustreExp
multiplyDivideLustreExpParser =
  do
    chainl1 specialLustreExpParser operator
  where
    operator :: Parser (LustreExp -> LustreExp -> LustreExp)
    operator =
      try
        ( do
            spaces
            string "*"
            spaces
            return LustreExpMultiply
        )
        <|> try
          ( do
              spaces
              string "/"
              spaces
              return LustreExpDivide
          )

specialLustreExpParser :: Parser LustreExp
specialLustreExpParser =
  try
    ( do
        char '('
        spaces
        e <- lustreExpParser
        spaces
        char ')'
        return e
    )
    <|> try
      ( do
          string "if"
          notFollowedBy alphaNum
          spaces
          e <- lustreExpParser
          spaces
          string "then"
          notFollowedBy alphaNum
          spaces
          e1 <- lustreExpParser
          spaces
          string "else"
          notFollowedBy alphaNum
          spaces
          e2 <- lustreExpParser
          return (LustreExpIf e e1 e2)
      )
    <|> try
      ( do
          string "true"
          notFollowedBy alphaNum
          return LustreExpTrue
      )
    <|> try
      ( do
          string "false"
          notFollowedBy alphaNum
          return LustreExpFalse
      )
    <|> try
      ( do
          n <- many1 digit
          notFollowedBy letter
          let num = read n :: Integer
          return (LustreExpInt num)
      )
    <|> try
      ( do
          v <- lustreVarParser
          notFollowedBy alphaNum
          return (LustreExpVar v)
      )
    <|> ( do
            string "pre"
            spaces
            char '('
            v <- lustreVarParser
            spaces
            char ')'
            return (LustreExpPre v)
        )

lustreVarParser :: Parser String
lustreVarParser =
  do
    c <- lower
    str <- many letter
    notFollowedBy alphaNum
    checkStr (c : str)
  where
    checkStr str =
      case str of
        "node" -> fail "node cannot be Lustre variable."
        "and" -> fail "and cannot be Lustre variable."
        "or" -> fail "or cannot be Lustre variable."
        "not" -> fail "not cannot be Lustre variable."
        "let" -> fail "let cannot be Lustre variable."
        "tel" -> fail "tel cannot be Lustre variable."
        "pre" -> fail "pre cannot be Lustre variable."
        "returns" -> fail "returns cannot be Lustre variable."
        "bool" -> fail "bool cannot be Lustre variable."
        "int" -> fail "int cannot be Lustre variable."
        "if" -> fail "if cannot be Lustre variable."
        "then" -> fail "then cannot be Lustre variable."
        "else" -> fail "else cannot be Lustre variable."
        _ -> return (str)

lustreReturnVarParser :: Parser (String, LustreType)
lustreReturnVarParser =
  do
    char '('
    spaces
    l <- singleArgParser
    spaces
    char ')'
    return l

lustreArgsParser :: Parser [(String, LustreType)]
lustreArgsParser =
  do
    char '('
    spaces
    l <- sepBy singleArgParser (try commaParser)
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

singleArgParser :: Parser (String, LustreType)
singleArgParser =
  do
    str <- lustreVarParser
    spaces
    char ':'
    spaces
    t <- lustreTypeParser
    return (str, t)

lustreTypeParser :: Parser LustreType
lustreTypeParser =
  try
    ( do
        string "bool"
        return LustreBool
    )
    <|> do
      string "int"
      return LustreInt