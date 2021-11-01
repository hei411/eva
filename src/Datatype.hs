module Datatype where

type Program = [Statement]

type TypeCheckedProgram = [(String, CExp, BType, [TypeProperty])]

type TypenameList = [(String, BType, Integer)]

data TypeProperty
  = Limit
  | Stable
  | None
  | Both
  deriving (Show, Eq)

type CompiledFilesData = [(FilePath, TypeCheckedProgram, TypenameList)]

data Statement
  = LetStatement String [(TypeProperty, String)] AExp
  | TypeStatement String [String] AType
  | ImportStatement String
  deriving (Show)

-- First Parse
data AExp
  = AExpVar String [AType]
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
  | AExpRec String AType AExp
  | AExpOut AExp
  | AExpInto AExp AType
  deriving (Show, Eq)

data AType
  = ATypeVar String
  | ATypeName String [AType]
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

-- solve parametric, convert index if needed, then perform type synonym conversion (Need to be super careful with indices!)
data BType
  = BTypeIndex Integer
  | BTypeParametric Integer TypeProperty
  | BTypeNameParam Integer
  | BTypeUnit
  | BTypeNat
  | BTypeProduct BType BType
  | BTypeSum BType BType
  | BTypeFunction BType BType
  | BTypeBox BType
  | BTypeArrow BType
  | BTypeAt BType
  | BTypeFix BType
  | BTypeUntil BType BType
  deriving (Show, Eq)

-- BExp are AExp except all type ascriptions are valid
data BExp
  = BExpVar String [BType]
  | BExpUnit
  | BExpLambda String BType BExp
  | BExpApplication BExp BExp
  | BExpProduct BExp BExp
  | BExpFst BExp
  | BExpSnd BExp
  | BExpInl BExp BType
  | BExpInr BExp BType
  | BExpMatch BExp String BExp String BExp
  | BExpZero
  | BExpSuc BExp
  | BExpPrimrec BExp BExp String String BExp
  | BExpArrow BExp
  | BExpAt BExp
  | BExpAdv BExp
  | BExpBox BExp
  | BExpUnbox BExp
  | BExpNow BExp BType
  | BExpWait BExp BExp
  | BExpUrec BExp String BExp String String String BExp
  | BExpRec String BType BExp
  | BExpOut BExp
  | BExpInto BExp BType
  deriving (Show, Eq)

-- CExp are for interpretation, i,e, no type ascriptions, function calls are substituted and db indices for expressions
data CExp
  = CExpIndex Integer
  | CExpUnit
  | CExpLambda CExp
  | CExpApplication CExp CExp
  | CExpProduct CExp CExp
  | CExpFst CExp
  | CExpSnd CExp
  | CExpInl CExp
  | CExpInr CExp
  | CExpMatch CExp CExp CExp
  | CExpZero
  | CExpSuc CExp
  | CExpPrimrec CExp CExp CExp
  | CExpArrow CExp
  | CExpAt CExp
  | CExpAdv CExp
  | CExpBox CExp
  | CExpUnbox CExp
  | CExpNow CExp
  | CExpWait CExp CExp
  | CExpUrec CExp CExp CExp
  | CExpRec CExp
  | CExpOut CExp
  | CExpInto CExp
  | CExpLocation Integer
  deriving (Show, Eq)

-- Type checking
type ContextElem = (String, BType)

type ContextElemList = [ContextElem]

data Context
  = TokenlessContext ContextElemList
  | StableContext ContextElemList ContextElemList
  | ArrowContext ContextElemList ContextElemList ContextElemList
  | AtContext ContextElemList ContextElemList ContextElemList

-- Interpreter
type StoreElem = (Integer, CExp)

type StoreElemList = [StoreElem]

data Store
  = NullStore
  | TicklessStore StoreElemList
  | TickStore StoreElemList StoreElemList