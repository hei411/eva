module Parser.ExpParser where

import Datatype
import Parser.TypeParser
import Parser.VarParser
import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec (notFollowedBy)
import Text.ParserCombinators.Parsec.Combinator (notFollowedBy)

inputParser :: Parser AExp
inputParser =
  ( do
      spaces
      exp <- expParser
      spaces
      eof
      return exp
  )

expParser :: Parser AExp
expParser =
  try letExpParser
    <|> try ifExpParser
    <|> try bindExpParser
    <|> orExpParser
    <|> fail "Cannot parse an expression"

letExpParser :: Parser AExp
letExpParser =
  try
    ( do
        string "let"
        notFollowedBy alphaNum
        spaces
        var <- varParser
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
        string "in"
        notFollowedBy alphaNum
        spaces
        body <- expParser
        let modifiedexp = modifyExp firstParameters pound secondParameters exp
        return (AExpLet var modifiedexp body)
    )

ifExpParser :: Parser AExp
ifExpParser =
  try
    ( do
        string "if"
        notFollowedBy alphaNum
        spaces
        e <- expParser
        spaces
        string "then"
        notFollowedBy alphaNum
        spaces
        e1 <- expParser
        spaces
        string "else"
        notFollowedBy alphaNum
        spaces
        e2 <- expParser
        return (AExpIf e e1 e2)
    )

modifyExp :: [(String, AType)] -> Maybe Char -> [(String, AType)] -> AExp -> AExp
modifyExp firstParameters pound secondParameters exp =
  addLambda firstParameters (addPound pound (addLambda secondParameters exp))

addPound :: Maybe Char -> AExp -> AExp
addPound p exp = case p of
  Nothing -> exp
  Just s -> AExpBox exp

lambdaParser :: Parser AExp
lambdaParser = do
  string "fun"
  notFollowedBy alphaNum
  spaces
  l <- many1 (annoVarParser)
  string "=>"
  spaces
  exp <- expParser
  return (addLambda l exp)

recParser :: Parser AExp
recParser = do
  string "rec"
  notFollowedBy alphaNum
  spaces
  (v, t) <- annoVarParser
  string "=>"
  spaces
  exp <- expParser
  return (AExpRec v t exp)

bindExpParser :: Parser AExp
bindExpParser =
  try lambdaParser
    <|> recParser
    <|> fail "Can't parse lambda abstraction or fix abstraction"

orExpParser :: Parser AExp
orExpParser =
  ( do
      e1 <- andExpParser
      e2 <- optionMaybe helper
      case e2 of
        Nothing -> return e1
        Just ae -> return (AExpOr e1 ae)
  )
  where
    helper :: Parser AExp
    helper =
      try
        ( do
            spaces
            string "or"
            notFollowedBy alphaNum
            spaces
            e2 <- orExpParser
            return e2
        )

andExpParser :: Parser AExp
andExpParser =
  ( do
      e1 <- notExpParser
      e2 <- optionMaybe helper
      case e2 of
        Nothing -> return e1
        Just ae -> return (AExpAnd e1 ae)
  )
  where
    helper :: Parser AExp
    helper =
      try
        ( do
            spaces
            string "and"
            notFollowedBy alphaNum
            spaces
            e2 <- andExpParser
            return e2
        )

notExpParser :: Parser AExp
notExpParser = do
  try
    ( do
        string "not"
        notFollowedBy alphaNum
        spaces
        exp <- notExpParser
        return (AExpNot exp)
    )
    <|> compareExpParser

compareExpParser :: Parser AExp
compareExpParser =
  try
    ( do
        e <- appExpMaybeBindingLastParser
        e1 <- optionMaybe helper1
        case e1 of
          Nothing ->
            do
              e2 <- optionMaybe helper2
              case e2 of
                Nothing -> return e
                Just ae -> return (AExpNotEquals e ae)
          Just ae -> return (AExpEquals e ae)
    )
  where
    helper1 :: Parser AExp
    helper1 =
      try
        ( do
            spaces
            string "=="
            spaces
            e2 <- appExpMaybeBindingLastParser
            return e2
        )
    helper2 :: Parser AExp
    helper2 =
      try
        ( do
            spaces
            string "!="
            spaces
            e2 <- appExpMaybeBindingLastParser
            return e2
        )

appExpMaybeBindingLastParser :: Parser AExp
appExpMaybeBindingLastParser =
  try
    ( do
        first <- appExpParser
        spaces
        second <- bindExpParser
        return (AExpApplication first second)
    )
    <|> appExpParser

appExpParser :: Parser AExp
appExpParser = do
  h <- oneExpParser
  t <-
    many
      ( try
          ( do
              spaces
              oneExpParser
          )
      )
  return (foldApplication (h : t))

foldApplication :: [AExp] -> AExp
foldApplication l = case l of
  [] -> error "Should not happen!"
  h : t -> foldApplicationHelper h t
  where
    foldApplicationHelper :: AExp -> [AExp] -> AExp
    foldApplicationHelper h t = case t of
      [] -> h
      hd : tl -> foldApplicationHelper (AExpApplication h hd) tl

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
  spaces
  second <- expParser
  spaces
  char ')'
  return (AExpProduct first second)

fstParser :: Parser AExp
fstParser = do
  string "fst"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpFst exp)

sndParser :: Parser AExp
sndParser = do
  string "snd"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpSnd exp)

inlParser :: Parser AExp
inlParser = do
  exp <- inlParserHelper
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpInl exp t)
  where
    inlParserHelper :: Parser AExp
    inlParserHelper =
      try
        ( do
            char '('
            spaces
            exp <- inlParserHelper
            spaces
            char ')'
            return exp
        )
        <|> do
          string "inl"
          notFollowedBy alphaNum
          spaces
          exp <- expParser
          spaces
          return exp

inrParser :: Parser AExp
inrParser = do
  exp <- inrParserHelper
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpInr exp t)
  where
    inrParserHelper :: Parser AExp
    inrParserHelper =
      try
        ( do
            char '('
            spaces
            exp <- inrParserHelper
            spaces
            char ')'
            return exp
        )
        <|> do
          string "inr"
          notFollowedBy alphaNum
          spaces
          exp <- expParser
          spaces
          return exp

matchParser :: Parser AExp
matchParser =
  try
    ( do
        string "match"
        skipMany1 space
        exp <- expParser
        spaces
        string "with"
        spaces
        char '|'
        spaces
        string "inl"
        skipMany1 space
        v1 <- varParser
        spaces
        string "=>"
        spaces
        exp1 <- expParser
        spaces
        char '|'
        spaces
        string "inr"
        skipMany1 space
        v2 <- varParser
        spaces
        string "=>"
        spaces
        exp2 <- expParser
        return (AExpMatch exp v1 exp1 v2 exp2)
    )
    <|> ( do
            string "match"
            skipMany1 space
            exp <- expParser
            spaces
            string "with"
            spaces
            char '|'
            spaces
            string "inr"
            skipMany1 space
            v2 <- varParser
            spaces
            string "=>"
            spaces
            exp2 <- expParser
            spaces
            char '|'
            spaces
            string "inl"
            skipMany1 space
            v1 <- varParser
            spaces
            string "=>"
            spaces
            exp1 <- expParser
            return (AExpMatch exp v1 exp1 v2 exp2)
        )

numberParser :: Parser AExp
numberParser = do
  n <- many1 digit
  notFollowedBy letter
  let num = read n :: Integer
  return (wrapSuc num)

wrapSuc :: Integer -> AExp
wrapSuc n =
  case n of
    0 -> AExpZero
    _ -> AExpSuc (wrapSuc (n -1))

sucParser :: Parser AExp
sucParser = do
  string "suc"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpSuc exp)

primrecParser :: Parser AExp
primrecParser =
  try
    ( do
        string "primrec"
        notFollowedBy alphaNum
        spaces
        exp <- expParser
        spaces
        string "with"
        spaces
        char '|'
        spaces
        char '0'
        spaces
        string "=>"
        spaces
        exp1 <- expParser
        spaces
        char '|'
        spaces
        string "suc"
        skipMany1 space
        v1 <- varParser
        spaces
        string ","
        spaces
        v2 <- varParser
        spaces
        string "=>"
        spaces
        exp2 <- expParser
        return (AExpPrimrec exp exp1 v1 v2 exp2)
    )
    <|> do
      string "primrec"
      notFollowedBy alphaNum
      spaces
      exp <- expParser
      spaces
      string "with"
      spaces
      char '|'
      spaces
      string "suc"
      skipMany1 space
      v1 <- varParser
      spaces
      string ","
      spaces
      v2 <- varParser
      spaces
      string "=>"
      spaces
      exp2 <- expParser
      spaces
      char '|'
      spaces
      char '0'
      spaces
      string "=>"
      spaces
      exp1 <- expParser
      return (AExpPrimrec exp exp1 v1 v2 exp2)

angleParser :: Parser AExp
angleParser = do
  char '>'
  spaces
  exp <- firstExpParser
  return (AExpAngle exp)

atParser :: Parser AExp
atParser = do
  char '@'
  spaces
  exp <- firstExpParser
  return (AExpAt exp)

advParser :: Parser AExp
advParser = do
  --string "adv"
  --notFollowedBy alphaNum
  char '<'
  spaces
  exp <- firstExpParser
  return (AExpAdv exp)

unboxParser :: Parser AExp
unboxParser = do
  --string "unbox"
  --notFollowedBy alphaNum
  char '?'
  spaces
  exp <- firstExpParser
  return (AExpUnbox exp)

boxParser :: Parser AExp
boxParser = do
  string "#"
  spaces
  exp <- firstExpParser
  return (AExpBox exp)

nowParser :: Parser AExp
nowParser = do
  exp <- nowParserHelper
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpNow exp t)
  where
    nowParserHelper :: Parser AExp
    nowParserHelper =
      try
        ( do
            char '('
            spaces
            exp <- nowParserHelper
            spaces
            char ')'
            return exp
        )
        <|> do
          string "now"
          notFollowedBy alphaNum
          spaces
          exp <- expParser
          spaces
          return exp

waitParser :: Parser AExp
waitParser = do
  string "wait"
  notFollowedBy alphaNum
  spaces
  exp1 <- oneExpParser
  spaces
  exp2 <- firstExpParser
  return (AExpWait exp1 exp2)

urecParser :: Parser AExp
urecParser =
  try
    ( do
        string "urec"
        notFollowedBy alphaNum
        spaces
        exp <- expParser
        spaces
        string "with"
        spaces
        char '|'
        spaces
        string "now"
        skipMany1 space
        v1 <- varParser
        spaces
        string "=>"
        spaces
        exp1 <- expParser
        spaces
        char '|'
        spaces
        string "wait"
        skipMany1 space
        v2 <- varParser
        skipMany1 space
        v3 <- varParser
        spaces
        string ","
        spaces
        v4 <- varParser
        spaces
        string "=>"
        spaces
        exp2 <- expParser
        return (AExpUrec exp v1 exp1 v2 v3 v4 exp2)
    )
    <|> do
      string "urec"
      notFollowedBy alphaNum
      spaces
      exp <- expParser
      spaces
      string "with"
      spaces
      char '|'
      spaces
      string "wait"
      skipMany1 space
      v2 <- varParser
      skipMany1 space
      v3 <- varParser
      spaces
      string ","
      spaces
      v4 <- varParser
      spaces
      string "=>"
      spaces
      exp2 <- expParser
      spaces
      char '|'
      spaces
      string "now"
      skipMany1 space
      v1 <- varParser
      spaces
      string "=>"
      spaces
      exp1 <- expParser
      return (AExpUrec exp v1 exp1 v2 v3 v4 exp2)

outParser :: Parser AExp
outParser = do
  string "out"
  notFollowedBy alphaNum
  spaces
  exp <- firstExpParser
  return (AExpOut exp)

intoParser :: Parser AExp
intoParser = do
  exp <- intoParserHelper
  spaces
  char ':'
  spaces
  t <- typeParser
  return (AExpInto exp t)
  where
    intoParserHelper :: Parser AExp
    intoParserHelper =
      try
        ( do
            char '('
            spaces
            exp <- intoParserHelper
            spaces
            char ')'
            return exp
        )
        <|> do
          string "into"
          notFollowedBy alphaNum
          spaces
          exp <- expParser
          spaces
          return exp

expVarParser :: Parser AExp
expVarParser = do
  str <- potentialDotVarParser
  parameters <- optionMaybe (try expParameterParser)
  case parameters of
    Nothing -> return (AExpVar str [])
    Just ats -> return (AExpVar str ats)

expParameterParser :: Parser [AType]
expParameterParser =
  do
    spaces
    char '['
    spaces
    l <- sepBy1 typeParser (try commaParser)
    spaces
    char ']'
    return l
  where
    commaParser :: Parser ()
    commaParser =
      do
        spaces
        char ','
        spaces

unitParser :: Parser AExp
unitParser = do
  char '('
  spaces
  char ')'
  return AExpUnit

trueParser :: Parser AExp
trueParser = do
  string "true"
  notFollowedBy alphaNum
  return AExpTrue

falseParser :: Parser AExp
falseParser = do
  string "false"
  notFollowedBy alphaNum
  return AExpFalse

oneExpParser :: Parser AExp
oneExpParser =
  try parenthesisParser
    <|> try productParser
    <|> try fstParser
    <|> try sndParser
    <|> try inlParser
    <|> try inrParser
    <|> try matchParser
    <|> try numberParser
    <|> try sucParser
    <|> try primrecParser
    <|> try angleParser
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
    <|> try unitParser
    <|> try trueParser
    <|> falseParser
    <|> fail "Can't parse OneExp"

annoVarParser :: Parser (String, AType)
annoVarParser =
  try
    ( do
        char '('
        spaces
        p <- annoVarParser
        spaces
        char ')'
        spaces
        return p
    )
    <|> try
      ( do
          s <- varParser
          spaces
          char ':'
          spaces
          t <- typeParser
          spaces
          return (s, t)
      )

addLambda :: [(String, AType)] -> AExp -> AExp
addLambda l exp = case l of
  [] -> exp
  (s, t) : tl -> AExpLambda s t (addLambda tl exp)