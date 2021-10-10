module Parser.VarParser where

import Text.Parsec
import Text.Parsec.String

varParser :: Parser String
varParser =
  try
    ( do
        char '('
        str <- varParser
        char ')'
        return str
    )
    <|> do
      start <- lower
      rest <- many alphaNum
      let str = start : rest
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
        _ -> return (start : rest)
