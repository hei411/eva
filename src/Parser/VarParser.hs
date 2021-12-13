module Parser.VarParser where

import Text.Parsec
import Text.Parsec.String

varParser :: Parser String
varParser =
  try
    ( do
        char '('
        spaces
        str <- varParser
        spaces
        char ')'
        return str
    )
    <|> do
      start <- lower
      rest <- many (choice [alphaNum, oneOf "_'"])
      let str = start : rest
      checkVar str

checkVar :: String -> Parser String
checkVar str =
  case str of
    "fun" -> fail "fun cannot be variable name."
    "fst" -> fail "fst cannot be variable name."
    "snd" -> fail "snd cannot be variable name."
    "inl" -> fail "inl cannot be variable name."
    "inr" -> fail "inr cannot be variable name."
    "suc" -> fail "suc cannot be variable name."
    "match" -> fail "match cannot be variable name."
    "with" -> fail "with cannot be variable name."
    "primrec" -> fail "primrec cannot be variable name."
    --"fby" -> fail "fby cannot be variable name."
    --"adv" -> fail "adv cannot be variable name."
    --"unbox" -> fail "unbox cannot be variable name."
    "now" -> fail "now cannot be variable name."
    "urec" -> fail "urec cannot be variable name."
    "rec" -> fail "rec cannot be variable name."
    "into" -> fail "into cannot be variable name."
    "out" -> fail "out cannot be variable name."
    "import" -> fail "import cannot be variable name."
    "let" -> fail "let cannot be variable name."
    "def" -> fail "def cannot be variable name."
    "in" -> fail "in cannot be variable name."
    "type" -> fail "type cannot be variable name."
    "as" -> fail "as cannot be variable name."
    "true" -> fail "true cannot be variable name."
    "false" -> fail "false cannot be variable name."
    "if" -> fail "if cannot be variable name."
    "then" -> fail "then cannot be variable name."
    "else" -> fail "else cannot be variable name."
    "and" -> fail "and cannot be variable name."
    "or" -> fail "or cannot be variable name."
    "not" -> fail "not cannot be variable name."
    _ -> return str

upperVarParser :: Parser String
upperVarParser =
  try
    ( do
        char '('
        spaces
        str <- upperVarParser
        spaces
        char ')'
        return str
    )
    <|> do
      start <- upper
      rest <- many (choice [alphaNum, oneOf "_'"])
      let str = start : rest
      checkUpperVar str

checkUpperVar :: String -> Parser String
checkUpperVar str = case str of
  "Fix" -> fail "Fix cannot be type variable name."
  "Until" -> fail "Until cannot be type variable name."
  "Nat" -> fail "Nat cannot be type variable name."
  "Unit" -> fail "Unit cannot be type variable name."
  "Stable" -> fail "Stable cannot be type variable name."
  "Limit" -> fail "Limit cannot be type variable name."
  "Bool" -> fail "Bool cannot be type variable name."
  "CStable" -> fail "CStable cannot be type variable name."
  _ -> return (str)

potentialDotVarParser :: Parser String
potentialDotVarParser =
  try
    ( do
        char '('
        spaces
        str <- potentialDotVarParser
        spaces
        char ')'
        return str
    )
    <|> try
      ( do
          starthd <- upper
          starttl <- many (choice [alphaNum, oneOf "_'"])
          c <- char '.'
          resthd <- lower
          resttl <- many (choice [alphaNum, oneOf "_'"])
          rest <- checkVar (resthd : resttl)
          return (starthd : starttl ++ ['.'] ++ rest)
      )
    <|> do
      start <- lower
      rest <- many (choice [alphaNum, oneOf "_'"])
      let str = start : rest
      notFollowedBy (char '.')
      checkVar str

potentialDotUpperVarParser :: Parser String
potentialDotUpperVarParser =
  try
    ( do
        char '('
        spaces
        str <- potentialDotUpperVarParser
        spaces
        char ')'
        return str
    )
    <|> try
      ( do
          starthd <- upper
          starttl <- many (choice [alphaNum, oneOf "_'"])
          c <- char '.'
          resthd <- upper
          resttl <- many (choice [alphaNum, oneOf "_'"])
          rest <- checkUpperVar (resthd : resttl)
          return (starthd : starttl ++ ['.'] ++ rest)
      )
    <|> do
      start <- upper
      rest <- many (choice [alphaNum, oneOf "_'"])
      let str = start : rest
      notFollowedBy (char '.')
      checkUpperVar str
