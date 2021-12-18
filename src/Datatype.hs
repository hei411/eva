module Datatype where

type Program = [Statement]

type TypeCheckedProgram = [(String, CExp, BType, [TypeProperty])]

type TypenameList = [(String, BType, Integer)]

data TypeProperty
  = Limit
  | Stable
  | CStable
  | None
  | Both
  | CBoth
  deriving (Show, Eq)

type CompiledFilesData = [(FilePath, TypeCheckedProgram, TypenameList)]

data Statement
  = DefStatement String [(TypeProperty, String)] AExp
  | TypeStatement String [String] AType
  | ImportStatement String (Maybe String)
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
  | AExpAngle AExp
  | AExpAt AExp
  | AExpAdv AExp
  | AExpBox AExp
  | AExpUnbox AExp
  | AExpNow AExp AType
  | AExpWait AExp AExp
  | AExpUrec AExp String AExp String String String AExp
  | AExpNfix String AType AExp
  | AExpOut AExp
  | AExpInto AExp AType
  | AExpLet String AExp AExp
  | AExpTrue
  | AExpFalse
  | AExpIf AExp AExp AExp 
  | AExpAnd AExp AExp 
  | AExpOr AExp AExp 
  | AExpNot AExp 
  | AExpEquals AExp AExp 
  | AExpNotEquals AExp AExp
  | AExpInteger Integer 
  | AExpIncrement AExp
  | AExpAdd AExp AExp 
  | AExpMinus AExp AExp 
  | AExpMultiply AExp AExp 
  | AExpDivide AExp AExp 
  | AExpMod AExp AExp 
  | AExpPower AExp AExp 
  | AExpStreamCons AExp AExp 
  | AExpLetStream String String AExp AExp
  | AExpEmptyList AType 
  | AExpNonEmptyList [AExp]
  | AExpListAppend AExp AExp
  | AExpListCons AExp AExp 
  | AExpListRec AExp AExp String String String AExp
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
  | ATypeAngle AType
  | ATypeAt AType
  | ATypeFix String AType
  | ATypeUntil AType AType
  | ATypeBool
  | ATypeList AType 
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
  | BTypeAngle BType
  | BTypeAt BType
  | BTypeFix BType
  | BTypeUntil BType BType
  | BTypeBool
  | BTypeList BType
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
  | BExpAngle BExp
  | BExpAt BExp
  | BExpAdv BExp
  | BExpBox BExp
  | BExpUnbox BExp
  | BExpNow BExp BType
  | BExpWait BExp BExp
  | BExpUrec BExp String BExp String String String BExp
  | BExpNfix String BType BExp
  | BExpOut BExp
  | BExpInto BExp BType
  | BExpLet String BExp BExp
  | BExpTrue
  | BExpFalse
  | BExpIf BExp BExp BExp 
  | BExpAnd BExp BExp 
  | BExpOr BExp BExp 
  | BExpNot BExp 
  | BExpEquals BExp BExp 
  | BExpNotEquals BExp BExp
  | BExpInteger Integer 
  | BExpIncrement BExp
  | BExpAdd BExp BExp 
  | BExpMinus BExp BExp 
  | BExpMultiply BExp BExp 
  | BExpDivide BExp BExp 
  | BExpMod BExp BExp 
  | BExpPower BExp BExp 
  | BExpStreamCons BExp BExp 
  | BExpLetStream String String BExp BExp
  | BExpEmptyList BType 
  | BExpNonEmptyList [BExp]
  | BExpListAppend BExp BExp
  | BExpListCons BExp BExp 
  | BExpListRec BExp BExp String String String BExp
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
  | CExpAdv CExp
  | CExpDelay CExp
  | CExpBox CExp
  | CExpUnbox CExp
  | CExpNow CExp
  | CExpWait CExp CExp
  | CExpUrec CExp CExp CExp
  | CExpNfix CExp
  | CExpOut CExp
  | CExpInto CExp
  | CExpLocation Integer
  | CExpTrue
  | CExpFalse
  | CExpIf CExp CExp CExp 
  | CExpAnd CExp CExp 
  | CExpOr CExp CExp 
  | CExpNot CExp 
  | CExpEquals CExp CExp 
  | CExpNotEquals CExp CExp
  | CExpInteger Integer 
  | CExpIncrement CExp
  | CExpAdd CExp CExp 
  | CExpMinus CExp CExp 
  | CExpMultiply CExp CExp 
  | CExpDivide CExp CExp 
  | CExpMod CExp CExp 
  | CExpPower CExp CExp 
  | CExpList [CExp]
  | CExpListAppend CExp CExp
  | CExpListCons CExp CExp 
  | CExpListRec CExp CExp CExp
  deriving (Show, Eq)
  -- | CExpAt CExp
  -- | CExpArrow CExp
  

-- Type checking
--Integer stores how many elements are in the var stack before pushing it in
type ContextElem = (String, BType, Integer)

type ContextElemList = [ContextElem]

data Context
  = TokenlessContext ContextElemList
  | StableContext ContextElemList ContextElemList
  | AngleContext ContextElemList ContextElemList ContextElemList
  | AtContext ContextElemList ContextElemList ContextElemList

-- Interpreter
data InterpreterMode
  = Normal
  | Safe
  | Lively
  | Fair
  | ISafe
  | ILively
  | IFair

type StoreElem = (Integer, CExp)

type StoreElemList = [StoreElem]

data Store
  = NullStore
  | TicklessStore StoreElemList
  | TickStore StoreElemList StoreElemList
  deriving Show