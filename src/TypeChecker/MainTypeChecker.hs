module TypeChecker.MainTypeChecker where

import Data.Functor.Contravariant (defaultEquivalence)
import Data.List
import Datatype
import PrintFunctions.BTypePrint
import PrintFunctions.CExpPrint
import TypeChecker.ContextFunctions
import TypeFunctions.ComparableChecker
import TypeFunctions.LimitChecker
import TypeFunctions.StableChecker
import TypeFunctions.SubstituteParametric
import TypeFunctions.TypeCompare
import TypeFunctions.TypePropertyChecker
import TypeFunctions.TypeUnfold

mainTypeChecker :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
mainTypeChecker file functionName definedFunctions context varStack bExp = case bExp of
  --varStack specifically for creating dbindices. Note that context may pop stuff out from context even we are not out of the range syntactically
  BExpVar s bts -> varRule file functionName definedFunctions context varStack s bts
  BExpUnit -> (CExpUnit, BTypeUnit)
  BExpLambda s bt be -> lambdaRule file functionName definedFunctions context varStack s bt be
  BExpApplication be be' -> applicationRule file functionName definedFunctions context varStack be be'
  BExpProduct be be' -> productRule file functionName definedFunctions context varStack be be'
  BExpFst be -> fstRule file functionName definedFunctions context varStack be
  BExpSnd be -> sndRule file functionName definedFunctions context varStack be
  BExpInl be bt -> inlRule file functionName definedFunctions context varStack be bt
  BExpInr be bt -> inrRule file functionName definedFunctions context varStack be bt
  BExpMatch be s be' str be2 -> matchRule file functionName definedFunctions context varStack be s be' str be2
  BExpZero -> (CExpZero, BTypeNat)
  BExpSuc be -> sucRule file functionName definedFunctions context varStack be
  BExpPrimrec be be' s str be2 -> primrecRule file functionName definedFunctions context varStack be be' s str be2
  BExpAngle be -> angleRule file functionName definedFunctions context varStack be
  BExpAt be -> atRule file functionName definedFunctions context varStack be
  BExpAdv be -> advRule file functionName definedFunctions context varStack be
  BExpBox be -> boxRule file functionName definedFunctions context varStack be
  BExpUnbox be -> unboxRule file functionName definedFunctions context varStack be
  BExpNow be bt -> nowRule file functionName definedFunctions context varStack be bt
  BExpWait be be' -> waitRule file functionName definedFunctions context varStack be be'
  BExpUrec be s be' str cs s' be2 -> urecRule file functionName definedFunctions context varStack be s be' str cs s' be2
  BExpRec s bt be -> recRule file functionName definedFunctions context varStack s bt be
  BExpOut be -> outRule file functionName definedFunctions context varStack be
  BExpInto be bt -> intoRule file functionName definedFunctions context varStack be bt
  BExpLet s be be' -> letRule file functionName definedFunctions context varStack s be be'
  BExpTrue -> (CExpTrue, BTypeBool)
  BExpFalse -> (CExpFalse, BTypeBool)
  BExpIf be be' be2 -> ifRule file functionName definedFunctions context varStack be be' be2
  BExpAnd be be' -> andRule file functionName definedFunctions context varStack be be'
  BExpOr be be' -> orRule file functionName definedFunctions context varStack be be'
  BExpNot be -> notRule file functionName definedFunctions context varStack be
  BExpEquals be be' -> equalsRule file functionName definedFunctions context varStack be be'
  BExpNotEquals be be' -> notEqualsRule file functionName definedFunctions context varStack be be'
  BExpInteger n -> (CExpInteger n, BTypeNat)
  BExpIncrement be -> incrementRule file functionName definedFunctions context varStack be
  BExpAdd be be' -> addRule file functionName definedFunctions context varStack be be'
  BExpMinus be be' -> minusRule file functionName definedFunctions context varStack be be'
  BExpMultiply be be' -> multiplyRule file functionName definedFunctions context varStack be be'
  BExpDivide be be' -> divideRule file functionName definedFunctions context varStack be be'
  BExpMod be be' -> modRule file functionName definedFunctions context varStack be be'
  BExpPower be be' -> powerRule file functionName definedFunctions context varStack be be'
  BExpPrepend be be' -> prependRule file functionName definedFunctions context varStack be be'
  BExpLetStream s s' be be' -> letStreamRule file functionName definedFunctions context varStack s s' be be'

varRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> String -> [BType] -> (CExp, BType)
varRule file functionName definedFunctions context varStack varName typeArguments =
  do
    let indexVarStack = elemIndex varName varStack
    case indexVarStack of
      Nothing -> varRuleNotInVarStack file functionName definedFunctions context varName typeArguments
      Just n -> varRuleInVarStack file functionName context (toInteger n) varName typeArguments

varRuleNotInVarStack :: FilePath -> String -> TypeCheckedProgram -> Context -> String -> [BType] -> (CExp, BType)
varRuleNotInVarStack file functionName definedFunctions context varName typeArguments =
  do
    let indexResult = findDefinedFunctions 0 definedFunctions
    case indexResult of
      Nothing -> typeCheckerErrorMsg file functionName ("Cannot resolve the variable " ++ varName)
      Just (index, cExp, bType, tp) ->
        if length tp /= length typeArguments
          then typeCheckerErrorMsg file functionName ("Wrong number of parametric parameters provided for " ++ varName)
          else do
            let typePropertyCorrect = checkTypeProperty tp typeArguments
            case typePropertyCorrect of
              Right (wrongType, intendedTP) ->
                typeCheckerErrorMsg file functionName ("The parametric parameter  " ++ show (wrongType) ++ " does not have type property " ++ show (intendedTP) ++ " for " ++ varName)
              Left () -> do
                let monoBType = substituteParametric bType typeArguments
                case context of
                  TokenlessContext x0 -> (cExp, monoBType)
                  StableContext x0 x1 ->
                    if isStable monoBType
                      then (cExp, monoBType)
                      else typeCheckerErrorMsg file functionName (varName ++ " is not stable type and is blocked by token(s) in Stable Context")
                  AngleContext x0 x1 x2 ->
                    if isStable monoBType
                      then (cExp, monoBType)
                      else typeCheckerErrorMsg file functionName (varName ++ " is not stable type and is blocked by token(s) in Angle Context")
                  AtContext x0 x1 x2 ->
                    if isStable monoBType
                      then (cExp, monoBType)
                      else typeCheckerErrorMsg file functionName (varName ++ " is not stable type and is blocked by token(s) in At Context")
  where
    findDefinedFunctions :: Integer -> TypeCheckedProgram -> Maybe (Integer, CExp, BType, [TypeProperty])
    findDefinedFunctions index functionList = case functionList of
      [] -> Nothing
      (name, cExp, bType, tp) : tl ->
        if name == varName
          then Just (index, cExp, bType, tp)
          else findDefinedFunctions (index + 1) tl

varRuleInVarStack :: FilePath -> String -> Context -> Integer -> String -> [BType] -> (CExp, BType)
varRuleInVarStack file functionName context n varName typeArguments =
  if length typeArguments > 0
    then typeCheckerErrorMsg file functionName ("Wrong number of parametric parameters provided for " ++ varName)
    else case context of
      TokenlessContext x0 -> do
        let foundType = elemContext x0 varName
        case foundType of
          Nothing -> typeCheckerErrorMsg file functionName (varName ++ " cannot be accessed in the tokenless context despite found in stack.")
          Just bt -> (CExpIndex n, bt)
      StableContext x0 x1 -> do
        let foundType = elemContext x1 varName
        case foundType of
          Nothing ->
            do
              let foundType2 = elemContext x0 varName
              case foundType2 of
                Nothing -> typeCheckerErrorMsg file functionName (varName ++ " cannot be accessed in the stable context despite found in stack.")
                Just bt ->
                  if isStable bt
                    then (CExpIndex n, bt)
                    else typeCheckerErrorMsg file functionName (varName ++ " cannot be accessed as it is not stable")
          Just bt -> (CExpIndex n, bt)
      AngleContext x0 x1 x2 -> do
        let foundType = elemContext x2 varName
        case foundType of
          Nothing ->
            do
              let foundType2 = elemContext (x1 ++ x0) varName
              case foundType2 of
                Nothing -> typeCheckerErrorMsg file functionName (varName ++ " cannot be accessed in the angle context despite found in stack.")
                Just bt ->
                  if isStable bt
                    then (CExpIndex n, bt)
                    else typeCheckerErrorMsg file functionName (varName ++ " cannot be accessed as it is not stable")
          Just bt -> (CExpIndex n, bt)
      AtContext x0 x1 x2 -> do
        let foundType = elemContext x2 varName
        case foundType of
          Nothing ->
            do
              let foundType2 = elemContext (x1 ++ x0) varName
              case foundType2 of
                Nothing -> typeCheckerErrorMsg file functionName (varName ++ " cannot be accessed in the at context despite found in stack.")
                Just bt ->
                  if isStable bt
                    then (CExpIndex n, bt)
                    else typeCheckerErrorMsg file functionName (varName ++ " cannot be accessed as it is not stable")
          Just bt -> (CExpIndex n, bt)

lambdaRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> String -> BType -> BExp -> (CExp, BType)
lambdaRule file functionName definedFunctions context varStack varName varType body = case context of
  AngleContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("Non-tick free context (AngleContext) in lambdaRule with arg " ++ varName)
  AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("Non-tick free context (AtContext) in lambdaRule with arg" ++ varName)
  _ ->
    do
      let (cBody, bodyType) = mainTypeChecker file functionName definedFunctions (addContextElem context (varName, varType)) (varName : varStack) body
      (CExpLambda cBody, BTypeFunction varType bodyType)

applicationRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
applicationRule file functionName definedFunctions context varStack e1 e2 =
  do
    let (c1, t1) = mainTypeChecker file functionName definedFunctions context varStack e1
    let (c2, t2) = mainTypeChecker file functionName definedFunctions context varStack e2
    case t1 of
      BTypeFunction a b ->
        if generalBTypeCompare a t2
          then (CExpApplication c1 c2, b)
          else
            typeCheckerErrorMsg
              file
              functionName
              ("applicationRule applied to two expression that are not compatible \n e1 = " ++ printCExp 0 c1 ++ " of type " ++ printBType 0 t1 ++ "\n e2 = " ++ printCExp 0 c2 ++ " of type" ++ printBType 0 t2)
      _ -> typeCheckerErrorMsg file functionName ("applicationRule applied to a non-function exp: " ++ printCExp 0 c1 ++ " of type " ++ printBType 0 t1)

productRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
productRule file functionName definedFunctions context varStack e1 e2 =
  do
    let (c1, t1) = mainTypeChecker file functionName definedFunctions context varStack e1
    let (c2, t2) = mainTypeChecker file functionName definedFunctions context varStack e2
    (CExpProduct c1 c2, BTypeProduct t1 t2)

fstRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
fstRule file functionName definedFunctions context varStack e =
  do
    let (c, t) = mainTypeChecker file functionName definedFunctions context varStack e
    case t of
      BTypeProduct p1 p2 -> (CExpFst c, p1)
      _ -> typeCheckerErrorMsg file functionName ("fstRule applied to a non-product type exp " ++ printCExp 0 c ++ " of type " ++ printBType 0 t)

sndRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
sndRule file functionName definedFunctions context varStack e =
  do
    let (c, t) = mainTypeChecker file functionName definedFunctions context varStack e
    case t of
      BTypeProduct p1 p2 -> (CExpSnd c, p2)
      _ -> typeCheckerErrorMsg file functionName ("sndRule applied to a non-product type exp " ++ printCExp 0 c ++ " of type " ++ printBType 0 t)

inlRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BType -> (CExp, BType)
inlRule file functionName definedFunctions context varStack e ascription =
  do
    let (c, t) = mainTypeChecker file functionName definedFunctions context varStack e
    case ascription of
      BTypeSum a b ->
        if generalBTypeCompare a t
          then (CExpInl c, ascription)
          else typeCheckerErrorMsg file functionName ("inlRule type ascription does not match intended inl type for exp " ++ printCExp 0 c)
      _ -> typeCheckerErrorMsg file functionName ("inlRule type ascription is not sum type for exp " ++ printCExp 0 c)

inrRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BType -> (CExp, BType)
inrRule file functionName definedFunctions context varStack e ascription =
  do
    let (c, t) = mainTypeChecker file functionName definedFunctions context varStack e
    case ascription of
      BTypeSum a b ->
        if generalBTypeCompare b t
          then (CExpInr c, ascription)
          else typeCheckerErrorMsg file functionName ("inrRule type ascription does not match intended inr type for exp " ++ printCExp 0 c)
      _ -> typeCheckerErrorMsg file functionName ("inrRule type ascription is not sum type for exp " ++ printCExp 0 c)

matchRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> String -> BExp -> String -> BExp -> (CExp, BType)
matchRule file functionName definedFunctions context varStack e inlVar e1 inrVar e2 =
  do
    let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
    case eType of
      BTypeSum a b ->
        do
          let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions (addContextElem context (inlVar, a)) (inlVar : varStack) e1
          let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions (addContextElem context (inrVar, b)) (inrVar : varStack) e2
          if generalBTypeCompare e1Type e2Type
            then (CExpMatch eExp e1Exp e2Exp, e1Type)
            else
              typeCheckerErrorMsg
                file
                functionName
                ( "matchRule branches type not compatible for exp " ++ printCExp 0 eExp
                    ++ "\n t1 = "
                    ++ printBType 0 e1Type
                    ++ "\n t2 = "
                    ++ printBType 0 e2Type
                )
      _ -> typeCheckerErrorMsg file functionName ("matchRule not applied to a sum type for exp " ++ printCExp 0 eExp)

sucRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
sucRule file functionName definedFunctions context varStack e =
  do
    let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
    case eType of
      BTypeNat -> (CExpSuc eExp, BTypeNat)
      _ -> typeCheckerErrorMsg file functionName ("sucRule applied to a non-Nat type for exp" ++ printCExp 0 eExp)

primrecRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> String -> String -> BExp -> (CExp, BType)
primrecRule file functionName definedFunctions context varStack e e1 var1 var2 e2 =
  do
    let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
    case eType of
      BTypeNat ->
        do
          let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
          let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions (addContextElem (addContextElem context (var1, BTypeNat)) (var2, e1Type)) (var2 : var1 : varStack) e2
          if generalBTypeCompare e1Type e2Type
            then (CExpPrimrec eExp e1Exp e2Exp, e1Type)
            else
              typeCheckerErrorMsg
                file
                functionName
                ( "primrecRule branches type not compatible for exp " ++ printCExp 0 eExp
                    ++ "\n t1 = "
                    ++ printBType 0 e1Type
                    ++ "\n t2 = "
                    ++ printBType 0 e2Type
                )
      _ -> typeCheckerErrorMsg file functionName ("primrecRule applied to a non-Nat type for exp " ++ printCExp 0 eExp)

angleRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
angleRule file functionName definedFunctions context varStack e =
  case context of
    TokenlessContext x0 -> typeCheckerErrorMsg file functionName ("angleRule applied to a non-stable context, i.e.  tokenless context for " ++ show (e))
    StableContext x0 x1 ->
      do
        let (eExp, eType) = mainTypeChecker file functionName definedFunctions (AngleContext x0 x1 []) varStack e
        (CExpDelay eExp, BTypeAngle eType)
    AngleContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("angleRule applied to a non-stable context, i.e.  angle context for " ++ show (e))
    AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("angleRule applied to a non-stable context, i.e.  atcontext for " ++ show (e))

atRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
atRule file functionName definedFunctions context varStack e =
  case context of
    TokenlessContext x0 -> typeCheckerErrorMsg file functionName ("atRule applied to a non-stable context, i.e.  tokenless context for " ++ show (e))
    StableContext x0 x1 ->
      do
        let (eExp, eType) = mainTypeChecker file functionName definedFunctions (AtContext x0 x1 []) varStack e
        (CExpDelay eExp, BTypeAt eType)
    AngleContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("atRule applied to a non-stable context, i.e.  angle context for " ++ show (e))
    AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("atRule applied to a non-stable context, i.e.  atcontext for " ++ show (e))

advRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
advRule file functionName definedFunctions context varStack e =
  case context of
    TokenlessContext x0 -> typeCheckerErrorMsg file functionName ("advRule applied to a non-tickful context, i.e.  tokenlesscontext for " ++ show (e))
    StableContext x0 x1 -> typeCheckerErrorMsg file functionName ("advRule applied to a non-tickful context, i.e.  stablecontext for " ++ show (e))
    AngleContext x0 x1 x2 ->
      do
        let (eExp, eType) = mainTypeChecker file functionName definedFunctions (StableContext x0 x1) varStack e
        case eType of
          BTypeAngle a -> (CExpAdv eExp, a)
          BTypeAt a -> (CExpAdv eExp, a)
          _ -> typeCheckerErrorMsg file functionName ("advRule applied to a non-delay type for exp" ++ printCExp 0 eExp ++ " of type " ++ printBType 0 eType)
    AtContext x0 x1 x2 ->
      do
        let (eExp, eType) = mainTypeChecker file functionName definedFunctions (StableContext x0 x1) varStack e
        case eType of
          BTypeAngle a ->
            if isLimit a
              then (CExpAdv eExp, a)
              else typeCheckerErrorMsg file functionName ("Failure to type a non-limit angle-delay type in an atcontext for exp" ++ printCExp 0 eExp ++ " of type " ++ printBType 0 eType)
          BTypeAt a -> (CExpAdv eExp, a)
          _ -> typeCheckerErrorMsg file functionName ("advRule applied to a non-delay type for exp" ++ printCExp 0 eExp ++ " of type " ++ printBType 0 eType)

boxRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
boxRule file functionName definedFunctions context varStack e = case context of
  TokenlessContext x0 ->
    do
      let (eExp, eType) = mainTypeChecker file functionName definedFunctions (StableContext x0 []) varStack e
      (CExpBox eExp, BTypeBox eType)
  StableContext x0 x1 -> typeCheckerErrorMsg file functionName ("boxRule applied to a non-tokenless context, i.e.  stablecontext for " ++ show e)
  AngleContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("boxRule applied to a non-tokenless context, i.e.  anglecontext for " ++ show e)
  AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("boxRule applied to a non-tokenless context, i.e.  AtContext for " ++ show e)

unboxRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
unboxRule file functionName definedFunctions context varStack e = case context of
  TokenlessContext x0 -> typeCheckerErrorMsg file functionName ("unboxRule applied to a tokenless context for " ++ show e)
  StableContext x0 x1 ->
    do
      let (eExp, eType) = mainTypeChecker file functionName definedFunctions (TokenlessContext x0) varStack e
      case eType of
        BTypeBox a -> (CExpUnbox eExp, a)
        _ -> typeCheckerErrorMsg file functionName ("boxRule applied to a non-boxed type for exp " ++ printCExp 0 eExp ++ " of type " ++ printBType 0 eType)
  AngleContext x0 x1 x2 ->
    do
      let (eExp, eType) = mainTypeChecker file functionName definedFunctions (TokenlessContext x0) varStack e
      case eType of
        BTypeBox a -> (CExpUnbox eExp, a)
        _ -> typeCheckerErrorMsg file functionName ("boxRule applied to a non-boxed type for exp " ++ printCExp 0 eExp ++ " of type " ++ printBType 0 eType)
  AtContext x0 x1 x2 ->
    do
      let (eExp, eType) = mainTypeChecker file functionName definedFunctions (TokenlessContext x0) varStack e
      case eType of
        BTypeBox a -> (CExpUnbox eExp, a)
        _ -> typeCheckerErrorMsg file functionName ("boxRule applied to a non-boxed type for exp " ++ printCExp 0 eExp ++ " of type " ++ printBType 0 eType)

nowRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BType -> (CExp, BType)
nowRule file functionName definedFunctions context varStack e ascription =
  do
    let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
    case ascription of
      BTypeUntil a b ->
        if generalBTypeCompare eType b
          then (CExpNow eExp, ascription)
          else typeCheckerErrorMsg file functionName ("nowRule type ascription does not match intended until type for exp " ++ printCExp 0 eExp)
      _ -> typeCheckerErrorMsg file functionName ("nowRule type ascription is not until type for exp " ++ printCExp 0 eExp)

waitRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
waitRule file functionName definedFunctions context varStack e1 e2 =
  do
    let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
    let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
    case e2Type of
      BTypeAt (BTypeUntil a b) ->
        if generalBTypeCompare a e1Type
          then (CExpWait e1Exp e2Exp, BTypeUntil a b)
          else typeCheckerErrorMsg file functionName ("waitRule two arguments do not match for " ++ printCExp 0 e1Exp ++ " and " ++ printCExp 0 e2Exp)
      _ -> typeCheckerErrorMsg file functionName ("waitRule's second argument is not an at-delayed Until type: " ++ printCExp 0 e2Exp)

urecRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> String -> BExp -> String -> String -> String -> BExp -> (CExp, BType)
urecRule file functionName definedFunctions context varStack e nowVar e1 waitVar1 waitVar2 nextVar e2 = case context of
  TokenlessContext x0 -> typeCheckerErrorMsg file functionName ("urecRule applied to a tokenless context for " ++ show (e))
  StableContext x0 x1 -> urecRuleHelper x0
  AngleContext x0 x1 x2 -> urecRuleHelper x0
  AtContext x0 x1 x2 -> urecRuleHelper x0
  where
    urecRuleHelper firstContextList =
      do
        let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
        case eType of
          BTypeUntil a b ->
            do
              let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions (StableContext firstContextList [(nowVar, b)]) (nowVar : varStack) e1
              let extendedContext = StableContext firstContextList [(nextVar, BTypeAt e1Type), (waitVar2, BTypeAt eType), (waitVar1, a)]
              let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions extendedContext (nextVar : waitVar2 : waitVar1 : varStack) e1
              if generalBTypeCompare e2Type e1Type
                then (CExpUrec eExp e1Exp e2Exp, e1Type)
                else
                  typeCheckerErrorMsg
                    file
                    functionName
                    ( "urecRule branches' types do not match for exp " ++ printCExp 0 eExp
                        ++ "\n t1 = "
                        ++ printBType 0 e1Type
                        ++ "\n t2 = "
                        ++ printBType 0 e2Type
                    )
          _ -> typeCheckerErrorMsg file functionName ("urecRule not applied to an until type exp" ++ printCExp 0 eExp)

recRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> String -> BType -> BExp -> (CExp, BType)
recRule file functionName definedFunctions context varStack var ascription e = case context of
  TokenlessContext x0 ->
    case ascription of
      BTypeBox a ->
        do
          let (eExp, eType) = mainTypeChecker file functionName definedFunctions (StableContext ((var, BTypeBox (BTypeAngle a)) : x0) []) (var : varStack) e
          if generalBTypeCompare eType a
            then (CExpRec eExp, ascription)
            else typeCheckerErrorMsg file functionName ("recRule type ascription does not match target type for exp " ++ printCExp 0 eExp)
      _ -> typeCheckerErrorMsg file functionName ("recRule type ascription is not box type for " ++ show e)
  StableContext x0 x1 -> typeCheckerErrorMsg file functionName ("recRule not applied in a tokenlessContext, i.e. StableContext for " ++ show e)
  AngleContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("recRule not applied in a tokenlessContext, i.e. AngleContext for " ++ show e)
  AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("recRule not applied in a tokenlessContext, i.e. AtContext for " ++ show e)

outRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
outRule file functionName definedFunctions context varStack e =
  do
    let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
    case eType of
      BTypeFix t ->
        (CExpOut eExp, unfoldBType eType)
      _ -> typeCheckerErrorMsg file functionName ("outRule not applied to an Fix type for exp " ++ printCExp 0 eExp)

intoRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BType -> (CExp, BType)
intoRule file functionName definedFunctions context varStack e ascription =
  case ascription of
    BTypeFix t ->
      do
        let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
        let unfoldFixType = unfoldBType ascription
        if generalBTypeCompare unfoldFixType eType
          then (CExpInto eExp, ascription)
          else typeCheckerErrorMsg file functionName ("intoRule ascription doesnt match inner expression type for exp " ++ printCExp 0 eExp)
    _ -> typeCheckerErrorMsg file functionName ("intoRule ascription is not Fix type for " ++ show e)

letRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> String -> BExp -> BExp -> (CExp, BType)
letRule file functionName definedFunctions context varStack str e body =
  -- Note. Currrently, we just simply rewriite let into a application rule. However I suspect this rule can be relaxed
  -- one needs to think... perhaps setting the body within a context with a tick is allowed, since the lambda abstraction is
  -- immediately applied?
  do
    let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
    let (cBody, bodyType) = mainTypeChecker file functionName definedFunctions (addContextElem context (str, eType)) (str : varStack) body
    seq eType (CExpApplication (CExpLambda cBody) eExp, bodyType)

{-seq used here for forcing type checking the arguments
Otherwise let x= ?2 = true will type check -}

{-case context of
  AngleContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("Currently we disallow let to be used in angle contexts for " ++ str ++ ". Ask Hei Li about this...")
  AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName ("Currently we disallow let to be used in at contexts for " ++ str ++ ". Ask Hei Li about this...")
  _ -> do
    let (_, eType) = mainTypeChecker file functionName definedFunctions context varStack exp
    let newExp = BExpApplication (BExpLambda str eType body) exp
    mainTypeChecker file functionName definedFunctions context varStack newExp-}

ifRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> BExp -> (CExp, BType)
ifRule file functionName definedFunctions context varStack e e1 e2 = do
  let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare eType BTypeBool
    then
      if generalBTypeCompare e1Type e2Type
        then (CExpIf eExp e1Exp e2Exp, e1Type)
        else
          typeCheckerErrorMsg
            file
            functionName
            ( "ifRule applied to non-matching branch types" ++ "\n t1 = "
                ++ printBType 0 e1Type
                ++ "\n t2 = "
                ++ printBType 0 e2Type
            )
    else typeCheckerErrorMsg file functionName ("ifRule not applied to a boolean predicate exp: " ++ printCExp 0 eExp)

andRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
andRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type BTypeBool
    then
      if generalBTypeCompare e2Type BTypeBool
        then (CExpAnd e1Exp e2Exp, BTypeBool)
        else typeCheckerErrorMsg file functionName ("andRule applied to a non-boolean second exp: " ++ printCExp 0 e2Exp)
    else typeCheckerErrorMsg file functionName ("andRule applied to a non-boolean first exp: " ++ printCExp 0 e1Exp)

orRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
orRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type BTypeBool
    then
      if generalBTypeCompare e2Type BTypeBool
        then (CExpOr e1Exp e2Exp, BTypeBool)
        else typeCheckerErrorMsg file functionName ("orRule applied to a non-boolean second exp: " ++ printCExp 0 e2Exp)
    else typeCheckerErrorMsg file functionName ("orRule applied to a non-boolean first exp: " ++ printCExp 0 e1Exp)

notRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
notRule file functionName definedFunctions context varStack e = do
  let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
  if generalBTypeCompare eType BTypeBool
    then (CExpNot eExp, BTypeBool)
    else typeCheckerErrorMsg file functionName ("notRule applied to a non-boolean exp: " ++ printCExp 0 eExp)

equalsRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
equalsRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type e2Type
    then
      if isComparable e1Type
        then (CExpEquals e1Exp e2Exp, BTypeBool)
        else
          typeCheckerErrorMsg
            file
            functionName
            ("equalsRule applied to a non-comparable type: " ++ printBType 0 e1Type ++ " and first exp: " ++ printCExp 0 e1Exp)
    else
      typeCheckerErrorMsg
        file
        functionName
        ("equalsRule applied to two different types: " ++ printBType 0 e1Type ++ " and " ++ printBType 0 e2Type)

notEqualsRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
notEqualsRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type e2Type
    then
      if isComparable e1Type
        then (CExpNotEquals e1Exp e2Exp, BTypeBool)
        else
          typeCheckerErrorMsg
            file
            functionName
            ("notEqualsRule applied to a non-comparable type: " ++ printBType 0 e1Type ++ " and first exp: " ++ printCExp 0 e1Exp)
    else
      typeCheckerErrorMsg
        file
        functionName
        ("notEqualsRule applied to two different types: " ++ printBType 0 e1Type ++ " and " ++ printBType 0 e2Type)

incrementRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> (CExp, BType)
incrementRule file functionName definedFunctions context varStack e =
  do
    let (eExp, eType) = mainTypeChecker file functionName definedFunctions context varStack e
    case eType of
      BTypeNat -> (CExpIncrement eExp, BTypeNat)
      _ -> typeCheckerErrorMsg file functionName ("incrementRule applied to a non-Nat type for exp" ++ printCExp 0 eExp)

addRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
addRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type BTypeNat
    then
      if generalBTypeCompare e2Type BTypeNat
        then (CExpAdd e1Exp e2Exp, BTypeNat)
        else typeCheckerErrorMsg file functionName ("addRule applied to a non-Nat second exp: " ++ printCExp 0 e2Exp)
    else typeCheckerErrorMsg file functionName ("addRule applied to a non-Nat first exp: " ++ printCExp 0 e1Exp)

minusRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
minusRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type BTypeNat
    then
      if generalBTypeCompare e2Type BTypeNat
        then (CExpMinus e1Exp e2Exp, BTypeNat)
        else typeCheckerErrorMsg file functionName ("minusRule applied to a non-Nat second exp: " ++ printCExp 0 e2Exp)
    else typeCheckerErrorMsg file functionName ("minusRule applied to a non-Nat first exp: " ++ printCExp 0 e1Exp)

multiplyRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
multiplyRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type BTypeNat
    then
      if generalBTypeCompare e2Type BTypeNat
        then (CExpMultiply e1Exp e2Exp, BTypeNat)
        else typeCheckerErrorMsg file functionName ("multiplyRule applied to a non-Nat second exp: " ++ printCExp 0 e2Exp)
    else typeCheckerErrorMsg file functionName ("multiplyRule applied to a non-Nat first exp: " ++ printCExp 0 e1Exp)

divideRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
divideRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type BTypeNat
    then
      if generalBTypeCompare e2Type BTypeNat
        then (CExpDivide e1Exp e2Exp, BTypeNat)
        else typeCheckerErrorMsg file functionName ("divideRule applied to a non-Nat second exp: " ++ printCExp 0 e2Exp)
    else typeCheckerErrorMsg file functionName ("divideRule applied to a non-Nat first exp: " ++ printCExp 0 e1Exp)

modRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
modRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type BTypeNat
    then
      if generalBTypeCompare e2Type BTypeNat
        then (CExpMod e1Exp e2Exp, BTypeNat)
        else typeCheckerErrorMsg file functionName ("modRule applied to a non-Nat second exp: " ++ printCExp 0 e2Exp)
    else typeCheckerErrorMsg file functionName ("modRule applied to a non-Nat first exp: " ++ printCExp 0 e1Exp)

powerRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
powerRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  if generalBTypeCompare e1Type BTypeNat
    then
      if generalBTypeCompare e2Type BTypeNat
        then (CExpPower e1Exp e2Exp, BTypeNat)
        else typeCheckerErrorMsg file functionName ("powerRule applied to a non-Nat second exp: " ++ printCExp 0 e2Exp)
    else typeCheckerErrorMsg file functionName ("powerRule applied to a non-Nat first exp: " ++ printCExp 0 e1Exp)

prependRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
prependRule file functionName definedFunctions context varStack e1 e2 = do
  let (e1Exp, e1Type) = mainTypeChecker file functionName definedFunctions context varStack e1
  let (e2Exp, e2Type) = mainTypeChecker file functionName definedFunctions context varStack e2
  let targetE2Type = BTypeAngle (BTypeFix (BTypeProduct e1Type (BTypeIndex 0)))
  if generalBTypeCompare targetE2Type e2Type
    then (CExpInto (CExpProduct e1Exp e2Exp), BTypeFix (BTypeProduct e1Type (BTypeIndex 0)))
    else typeCheckerErrorMsg file functionName ("prependRule applied to non-compatible expresions of type: " ++ printBType 0 e1Type ++ " and " ++ printBType 0 e2Type)

letStreamRule :: FilePath -> String -> TypeCheckedProgram -> Context -> [String] -> String -> String -> BExp -> BExp -> (CExp, BType)
letStreamRule file functionName definedFunctions context varStack var1 var2 e1 e2 = do
  let var1Exp = BExpFst (BExpOut (BExpVar "_Stream" []))
  let var2Exp = BExpSnd (BExpOut (BExpVar "_Stream" []))
  let newExp = BExpLet "_Stream" e1 (BExpLet var1 var1Exp (BExpLet var2 var2Exp e2))
  mainTypeChecker file functionName definedFunctions context varStack newExp

typeCheckerErrorMsg :: FilePath -> String -> [Char] -> (CExp, BType)
typeCheckerErrorMsg file functionName msg =
  error (file ++ ": " ++ msg ++ " in \"" ++ functionName ++ "\"")
