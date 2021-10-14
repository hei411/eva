module Datatype where

type Program = [Statement]

type TypeCheckedProgram = [(String, AExp, AType)]

data Statement = LetStatement String AExp deriving (Show)

data AExp
  = AExpVar String
  | AExpUnit
  | AExpLambda String AType AExp
  | AExpApplication AExp AExp
  | AExpProduct AExp AExp
  | AExpFst AExp
  | AExpSnd AExp
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
  | --Special exp for interpreter
    AExpLocation Integer
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
  | ATypeApplication AType AType
  deriving (Show, Eq)

type ContextElem = (String, AType)

type ContextElemList = [ContextElem]

data Context
  = TokenlessContext ContextElemList
  | StableContext ContextElemList ContextElemList
  | ArrowContext ContextElemList ContextElemList ContextElemList
  | AtContext ContextElemList ContextElemList ContextElemList

type StoreElem = (Integer, AExp)

type StoreElemList = [StoreElem]

data Store
  = NullStore
  | TicklessStore StoreElemList
  | TickStore StoreElemList StoreElemList