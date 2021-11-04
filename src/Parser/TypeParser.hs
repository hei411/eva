module Parser.TypeParser where

import Datatype
import Parser.VarParser
import Text.Parsec
import Text.Parsec.String

typeVarParser :: Parser AType
typeVarParser = do
  str <- potentialDotVarParser
  -- TODO: Using <> for arguments
  return (ATypeVar str)

typeNameParser :: Parser AType
typeNameParser = do
  str <- potentialDotUpperVarParser
  parameters <- optionMaybe (try typeNameParameterParser)
  case parameters of
    Nothing -> return (ATypeName str [])
    Just ats -> return (ATypeName str ats)

typeNameParameterParser :: Parser [(AType)]
typeNameParameterParser =
  do
    spaces
    char '('
    spaces
    l <- sepBy1 typeParser (try commaParser)
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

typeParser :: Parser AType
typeParser = do
  try fixTypeParser
    <|> type0Parser
    <|> fail "Can't parse type"

fixTypeParser :: Parser AType
fixTypeParser = do
  string "Fix"
  skipMany1 space
  v <- varParser
  spaces
  string "-->"
  spaces
  t <- typeParser
  return (ATypeFix v t)

type0Parser :: Parser AType
type0Parser =
  try
    ( do
        t1 <- type1'Parser
        spaces
        string "->"
        spaces
        t2 <- typeParser
        return (ATypeFunction t1 t2)
    )
    <|> type1Parser
    <|> fail "Can't parse type0"

type1Parser :: Parser AType
type1Parser =
  try
    ( do
        t1 <- type2'Parser
        spaces
        string "Until"
        notFollowedBy alphaNum
        spaces
        t2 <- type1Parser
        return (ATypeUntil t1 t2)
    )
    <|> try
      ( do
          t1 <- type2'Parser
          spaces
          string "Until"
          notFollowedBy alphaNum
          spaces
          t2 <- fixTypeParser
          return (ATypeUntil t1 t2)
      )
    <|> try type2Parser
    <|> type1'Parser
    <|> fail "Can't parse type1"

type1'Parser :: Parser AType
type1'Parser =
  try
    ( do
        t1 <- type2'Parser
        skipMany1 space
        string "Until"
        notFollowedBy alphaNum
        spaces
        t2 <- type1'Parser
        return (ATypeUntil t1 t2)
    )
    <|> type2'Parser
    <|> fail "Can't parse type1"

type2Parser :: Parser AType
type2Parser =
  try
    ( do
        t1 <- type2'Parser
        spaces
        char '+'
        spaces
        t2 <- type3Parser
        return (ATypeSum t1 t2)
    )
    <|> type3Parser
    <|> fail "Can't parse type2"

type2'Parser :: Parser AType
type2'Parser =
  do
    h <- type3'Parser
    t <-
      ( many
          ( try
              ( do
                  spaces
                  char '+'
                  spaces
                  type3'Parser
              )
          )
        )
    return (foldType2' (h : t))

foldType2' :: [AType] -> AType
foldType2' l = case l of
  [] -> error "Should not happen!"
  h : t -> foldApplicationHelper h t
  where
    foldApplicationHelper :: AType -> [AType] -> AType
    foldApplicationHelper h t = case t of
      [] -> h
      hd : tl -> foldApplicationHelper (ATypeSum h hd) tl

type3Parser :: Parser AType
type3Parser =
  try
    ( do
        t1 <- type3'Parser
        spaces
        char '*'
        spaces
        t2 <- type4Parser
        return (ATypeProduct t1 t2)
    )
    <|> type4Parser
    <|> fail "Can't parse type3"

type3'Parser :: Parser AType
type3'Parser = do
  h <- type4'Parser
  t <-
    ( many
        ( try
            ( do
                spaces
                char '*'
                spaces
                type4'Parser
            )
        )
      )
  return (foldType3' (h : t))

foldType3' :: [AType] -> AType
foldType3' l = case l of
  [] -> error "Should not happen!"
  h : t -> foldApplicationHelper h t
  where
    foldApplicationHelper :: AType -> [AType] -> AType
    foldApplicationHelper h t = case t of
      [] -> h
      hd : tl -> foldApplicationHelper (ATypeProduct h hd) tl

-- removed since we replace lambda and application with <> for type synonyms
{-
type4Parser :: Parser AType
type4Parser =
  try
    ( do
        t1 <- type4'Parser
        skipMany1 space
        t2 <- type5Parser
        return (ATypeApplication t1 t2)
    )
    <|> type5Parser
    <|> fail "Can't parse type4"

type4'Parser :: Parser AType
type4'Parser = do
  h <- type5'Parser
  t <-
    ( many
        ( try
            ( do
                skipMany1 space
                type5'Parser
            )
        )
      )
  return (foldType4' (h : t))

foldType4' :: [AType] -> AType
foldType4' l = case l of
  [] -> error "Should not happen!"
  h : t -> foldApplicationHelper h t
  where
    foldApplicationHelper :: AType -> [AType] -> AType
    foldApplicationHelper h t = case t of
      [] -> h
      hd : tl -> foldApplicationHelper (ATypeApplication h hd) tl

-}
firstTypeParser :: Parser AType
firstTypeParser =
  try fixTypeParser
    <|> type4Parser
    <|> fail "Can't parse FirstType"

type4Parser :: Parser AType
type4Parser =
  try
    ( do
        char '@'
        spaces
        t <- type4Parser
        return (ATypeAt t)
    )
    <|> try
      ( do
          char '>'
          spaces
          t <- type4Parser
          return (ATypeArrow t)
      )
    <|> try
      ( do
          string "#"
          spaces
          t <- type4Parser
          return (ATypeBox t)
      )
    <|> fixTypeParser
    <|> fail "Can't parse Type4"

type4'Parser :: Parser AType
type4'Parser =
  try
    ( do
        string "Unit"
        notFollowedBy alphaNum
        return ATypeUnit
    )
    <|> try
      ( do
          char '('
          spaces
          t <- typeParser
          spaces
          char ')'
          return t
      )
    <|> try
      ( do
          char '@'
          spaces
          t <- type4'Parser
          return (ATypeAt t)
      )
    <|> try
      ( do
          char '>'
          spaces
          t <- type4'Parser
          return (ATypeArrow t)
      )
    <|> try
      ( do
          string "#"
          spaces
          t <- type4'Parser
          return (ATypeBox t)
      )
    <|> try typeVarParser
    <|> try typeNameParser
    <|> ( do
            string "Nat"
            notFollowedBy alphaNum
            return ATypeNat
        )
    <|> fail "Can't parse Type4'"
