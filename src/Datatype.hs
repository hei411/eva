module Datatype where

type Program = [Statement]

data Statement = LetStatement String AExp deriving (Show)

data AExp
  = AExpVar String
  | AExpUnit
  | AExpLambda String AType AExp
  | AExpApplication AExp AExp
  | AExpProduct AExp AExp
  | AExpFst AExp
  | AExpInl AExp AType
  | AExpInr AExp AType
  | AExpMatch AExp String AExp String AExp
  | AExpZero
  | AExpSuc AExp
  | AExpPrimrec AExp AExp String String AExp
  | AExpArrow AExp
  | AExpAt AExp
  | AExpAdv AExp
  | AExpBox AExp
  | AExpUnbox AExp
  | AExpNow AExp AType
  | AExpWait AExp AExp
  | AExpUrec AExp String AExp String String String AExp
  | AExpFix String AType AExp
  | AExpOut AExp
  | AExpInto AExp AType
  deriving (Show, Eq)

data AType
  = ATypeVar String
  | ATypeUnit
  | ATypeNat
  | ATypeProduct AType AType
  | ATypeSum AType AType
  | ATypeFunction AType AType
  | ATypeBox AType
  | ATypeArrow AType
  | ATypeAt AType
  | ATypeFix String AType
  | ATypeUntil AType AType
  deriving (Show, Eq)
