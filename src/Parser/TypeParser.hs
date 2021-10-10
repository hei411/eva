module Parser.TypeParser where

import Datatype
import Parser.VarParser
import Text.Parsec
import Text.Parsec.String

typeVarParser :: Parser AType
typeVarParser = do
  str <- varParser
  return (ATypeVar str)

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
  string "->"
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
        skipMany1 space
        string "Until"
        skipMany1 space
        t2 <- type1Parser
        return (ATypeUntil t1 t2)
    )
    <|> try
      ( do
          t1 <- type2'Parser
          skipMany1 space
          string "Until"
          skipMany1 space
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
        skipMany1 space
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
        t2 <- fixTypeParser
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

--do
--  l <-
--    sepBy1
--      (type3'Parser)
--     ( try
--( do
--             spaces
--         char '+'
--         spaces
--     )
--   )
--  return (foldType2' l)

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
        t2 <- fixTypeParser
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

-- do
--  l <-
--  sepBy1
--    (type4'Parser)
--    ( try
--       ( do
--           spaces
--         char '*'
--       spaces
-- )
--      )
-- return (foldType3' l)

foldType3' :: [AType] -> AType
foldType3' l = case l of
  [] -> error "Should not happen!"
  h : t -> foldApplicationHelper h t
  where
    foldApplicationHelper :: AType -> [AType] -> AType
    foldApplicationHelper h t = case t of
      [] -> h
      hd : tl -> foldApplicationHelper (ATypeProduct h hd) tl

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

firstTypeParser :: Parser AType
firstTypeParser =
  try fixTypeParser
    <|> type5Parser
    <|> fail "Can't parse FirstType"

type5Parser :: Parser AType
type5Parser =
  try
    ( do
        char '@'
        spaces
        t <- fixTypeParser
        return (ATypeAt t)
    )
    <|> try
      ( do
          char '>'
          spaces
          t <- fixTypeParser
          return (ATypeArrow t)
      )
    <|> ( do
            string "[]"
            spaces
            t <- fixTypeParser
            return (ATypeBox t)
        )
    <|> fail "Can't parse Type5"

type5'Parser :: Parser AType
type5'Parser =
  try
    ( do
        string "Unit"
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
          t <- type5'Parser
          return (ATypeAt t)
      )
    <|> try
      ( do
          char '>'
          spaces
          t <- type5'Parser
          return (ATypeArrow t)
      )
    <|> try
      ( do
          string "[]"
          spaces
          t <- type5'Parser
          return (ATypeBox t)
      )
    <|> try typeVarParser
    <|> ( do
            string "Nat"
            return ATypeNat
        )
    <|> fail "Can't parse Type5"
