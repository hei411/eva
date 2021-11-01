module TypeChecker.MainTypeChecker where

import Datatype
import TypeChecker.ContextFunctions
import TypeChecker.LimitChecker
import TypeChecker.StableChecker
import TypeFunctions.TypeCompare
import TypeFunctions.TypeUnfold

mainTypeChecker :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
mainTypeChecker file functionName context varStack bExp = case bExp of
  --varStack specifically for creating dbindices. Note that context may pop stuff out from context even we are not out of the range syntactically
  BExpVar s bts -> varRule file functionName context varStack s bts
  BExpUnit -> (CExpUnit, BTypeUnit)
  BExpLambda s bt be -> lambdaRule file functionName context varStack s bt be
  BExpApplication be be' -> applicationRule file functionName context varStack be be'
  BExpProduct be be' -> productRule file functionName context varStack be be'
  BExpFst be -> fstRule file functionName context varStack be
  BExpSnd be -> sndRule file functionName context varStack be
  BExpInl be bt -> inlRule file functionName context varStack be bt
  BExpInr be bt -> inrRule file functionName context varStack be bt
  BExpMatch be s be' str be2 -> matchRule file functionName context varStack be s be' str be2
  BExpZero -> (CExpZero, BTypeNat)
  BExpSuc be -> sucRule file functionName context varStack be
  BExpPrimrec be be' s str be2 -> primrecRule file functionName context varStack be be' s str be2
  BExpArrow be -> arrowRule file functionName context varStack be
  BExpAt be -> atRule file functionName context varStack be
  BExpAdv be -> advRule file functionName context varStack be
  BExpBox be -> boxRule file functionName context varStack be
  BExpUnbox be -> unboxRule file functionName context varStack be
  BExpNow be bt -> nowRule file functionName context varStack be bt
  BExpWait be be' -> waitRule file functionName context varStack be be'
  BExpUrec be s be' str cs s' be2 -> urecRule file functionName context varStack be s be' str cs s' be2
  BExpRec s bt be -> recRule file functionName context varStack s bt be
  BExpOut be -> outRule file functionName context varStack be
  BExpInto be bt -> intoRule file functionName context varStack be bt

varRule :: FilePath -> String -> Context -> [String] -> String -> [BType] -> (CExp, BType)
varRule file functionName context varStack varName typeArguments = error "varRule not implemented"

lambdaRule :: FilePath -> String -> Context -> [String] -> String -> BType -> BExp -> (CExp, BType)
lambdaRule file functionName context varStack varName varType body = case context of
  ArrowContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "Non-tick free context (ArrowContext) in lambdaRule"
  AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "Non-tick free context (AtContext) in lambdaRule"
  _ ->
    do
      let (cBody, bodyType) = mainTypeChecker file functionName (addContextElem context (varName, varType, Nothing)) (varName : varStack) body
      (CExpLambda cBody, BTypeFunction varType bodyType)

applicationRule :: FilePath -> String -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
applicationRule file functionName context varStack e1 e2 =
  do
    let (c1, t1) = mainTypeChecker file functionName context varStack e1
    let (c2, t2) = mainTypeChecker file functionName context varStack e2
    case t1 of
      BTypeFunction a b ->
        if generalBTypeCompare a t2
          then (CExpApplication c1 c2, b)
          else typeCheckerErrorMsg file functionName "applicationRule applied to two expression that are not compatible"
      _ -> typeCheckerErrorMsg file functionName "applicationRule applied to a non-function type"

productRule :: FilePath -> String -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
productRule file functionName context varStack e1 e2 =
  do
    let (c1, t1) = mainTypeChecker file functionName context varStack e1
    let (c2, t2) = mainTypeChecker file functionName context varStack e2
    (CExpProduct c1 c2, BTypeProduct t1 t2)

fstRule :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
fstRule file functionName context varStack e =
  do
    let (c, t) = mainTypeChecker file functionName context varStack e
    case t of
      BTypeProduct p1 p2 -> (CExpFst c, p1)
      _ -> typeCheckerErrorMsg file functionName "fstRule applied to a non-product type"

sndRule :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
sndRule file functionName context varStack e =
  do
    let (c, t) = mainTypeChecker file functionName context varStack e
    case t of
      BTypeProduct p1 p2 -> (CExpSnd c, p2)
      _ -> typeCheckerErrorMsg file functionName "sndRule applied to a non-product type"

inlRule :: FilePath -> String -> Context -> [String] -> BExp -> BType -> (CExp, BType)
inlRule file functionName context varStack e ascription =
  do
    let (c, t) = mainTypeChecker file functionName context varStack e
    case ascription of
      BTypeSum a b ->
        if generalBTypeCompare a t
          then (CExpInl c, ascription)
          else typeCheckerErrorMsg file functionName "inlRule type ascription does not match intended inl type"
      _ -> typeCheckerErrorMsg file functionName "inlRule type ascription is not sum type"

inrRule :: FilePath -> String -> Context -> [String] -> BExp -> BType -> (CExp, BType)
inrRule file functionName context varStack e ascription =
  do
    let (c, t) = mainTypeChecker file functionName context varStack e
    case ascription of
      BTypeSum a b ->
        if generalBTypeCompare b t
          then (CExpInr c, ascription)
          else typeCheckerErrorMsg file functionName "inrRule type ascription does not match intended inr type"
      _ -> typeCheckerErrorMsg file functionName "inrRule type ascription is not sum type"

matchRule :: FilePath -> String -> Context -> [String] -> BExp -> String -> BExp -> String -> BExp -> (CExp, BType)
matchRule file functionName context varStack e inlVar e1 inrVar e2 =
  do
    let (eExp, eType) = mainTypeChecker file functionName context varStack e
    case eType of
      BTypeSum a b ->
        do
          let (e1Exp, e1Type) = mainTypeChecker file functionName (addContextElem context (inlVar, a, Nothing)) (inlVar : varStack) e1
          let (e2Exp, e2Type) = mainTypeChecker file functionName (addContextElem context (inrVar, b, Nothing)) (inrVar : varStack) e2
          if generalBTypeCompare e1Type e2Type
            then (CExpMatch eExp e1Exp e2Exp, e1Type)
            else typeCheckerErrorMsg file functionName "matchRule branches type not compatible"
      _ -> typeCheckerErrorMsg file functionName "matchRule not applied to a sum type"

sucRule :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
sucRule file functionName context varStack e =
  do
    let (eExp, eType) = mainTypeChecker file functionName context varStack e
    case eType of
      BTypeNat -> (CExpSuc eExp, BTypeNat)
      _ -> typeCheckerErrorMsg file functionName "sucRule applied to a non-Nat type"

primrecRule :: FilePath -> String -> Context -> [String] -> BExp -> BExp -> String -> String -> BExp -> (CExp, BType)
primrecRule file functionName context varStack e e1 var1 var2 e2 =
  do
    let (eExp, eType) = mainTypeChecker file functionName context varStack e
    case eType of
      BTypeNat ->
        do
          let (e1Exp, e1Type) = mainTypeChecker file functionName context varStack e1
          let (e2Exp, e2Type) = mainTypeChecker file functionName (addContextElem (addContextElem context (var1, BTypeNat, Nothing)) (var2, e1Type, Nothing)) (var2 : var1 : varStack) e2
          if generalBTypeCompare e1Type e2Type then (CExpPrimrec eExp e1Exp e2Exp, e1Type) else typeCheckerErrorMsg file functionName "primrecRule branches type not compatible"
      _ -> typeCheckerErrorMsg file functionName "primrecRule applied to a non-Nat type"

arrowRule :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
arrowRule file functionName context varStack e =
  case context of
    TokenlessContext x0 -> typeCheckerErrorMsg file functionName "arrowRule applied to a non-stable context, i.e.  tokenless context"
    StableContext x0 x1 ->
      do
        let (eExp, eType) = mainTypeChecker file functionName (ArrowContext x0 x1 []) varStack e
        (CExpArrow eExp, BTypeArrow eType)
    ArrowContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "arrowRule applied to a non-stable context, i.e.  arrow context"
    AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "arrowRule applied to a non-stable context, i.e.  atcontext"

atRule :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
atRule file functionName context varStack e =
  case context of
    TokenlessContext x0 -> typeCheckerErrorMsg file functionName "atRule applied to a non-stable context, i.e.  tokenless context"
    StableContext x0 x1 ->
      do
        let (eExp, eType) = mainTypeChecker file functionName (AtContext x0 x1 []) varStack e
        (CExpAt eExp, BTypeAt eType)
    ArrowContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "atRule applied to a non-stable context, i.e.  arrow context"
    AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "atRule applied to a non-stable context, i.e.  atcontext"

advRule :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
advRule file functionName context varStack e =
  case context of
    TokenlessContext x0 -> typeCheckerErrorMsg file functionName "advRule applied to a non-tickful context, i.e.  tokenlesscontext"
    StableContext x0 x1 -> typeCheckerErrorMsg file functionName "advRule applied to a non-tickful context, i.e.  stablecontext"
    ArrowContext x0 x1 x2 ->
      do
        let (eExp, eType) = mainTypeChecker file functionName (StableContext x0 x1) varStack e
        case eType of
          BTypeArrow a -> (CExpAdv eExp, a)
          BTypeAt a -> (CExpAdv eExp, a)
          _ -> typeCheckerErrorMsg file functionName "advRule applied to a non-delay type"
    AtContext x0 x1 x2 ->
      do
        let (eExp, eType) = mainTypeChecker file functionName (StableContext x0 x1) varStack e
        case eType of
          BTypeArrow a ->
            if isLimit a
              then (CExpAdv eExp, a)
              else typeCheckerErrorMsg file functionName "Failure to type a non-limit arrow-delay type in an atcontext"
          BTypeAt a -> (CExpAdv eExp, a)
          _ -> typeCheckerErrorMsg file functionName "advRule applied to a non-delay type"

boxRule :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
boxRule file functionName context varStack e = case context of
  TokenlessContext x0 ->
    do
      let (eExp, eType) = mainTypeChecker file functionName (StableContext x0 []) varStack e
      (CExpBox eExp, BTypeBox eType)
  StableContext x0 x1 -> typeCheckerErrorMsg file functionName "boxRule applied to a non-tokenless context, i.e.  stablecontext"
  ArrowContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "boxRule applied to a non-tokenless context, i.e.  ArrowContext"
  AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "boxRule applied to a non-tokenless context, i.e.  AtContext"

unboxRule :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
unboxRule file functionName context varStack e = case context of
  TokenlessContext x0 -> typeCheckerErrorMsg file functionName "unboxRule applied to a tokenless context"
  StableContext x0 x1 ->
    do
      let (eExp, eType) = mainTypeChecker file functionName (TokenlessContext x0) varStack e
      case eType of
        BTypeBox a -> (CExpUnbox eExp, a)
        _ -> typeCheckerErrorMsg file functionName "boxRule applied to a non-boxed type"
  ArrowContext x0 x1 x2 ->
    do
      let (eExp, eType) = mainTypeChecker file functionName (TokenlessContext x0) varStack e
      case eType of
        BTypeBox a -> (CExpUnbox eExp, a)
        _ -> typeCheckerErrorMsg file functionName "boxRule applied to a non-boxed type"
  AtContext x0 x1 x2 ->
    do
      let (eExp, eType) = mainTypeChecker file functionName (TokenlessContext x0) varStack e
      case eType of
        BTypeBox a -> (CExpUnbox eExp, a)
        _ -> typeCheckerErrorMsg file functionName "boxRule applied to a non-boxed type"

nowRule :: FilePath -> String -> Context -> [String] -> BExp -> BType -> (CExp, BType)
nowRule file functionName context varStack e ascription =
  do
    let (eExp, eType) = mainTypeChecker file functionName context varStack e
    case ascription of
      BTypeUntil a b ->
        if generalBTypeCompare eType b
          then (CExpNow eExp, ascription)
          else typeCheckerErrorMsg file functionName "nowRule type ascription does not match intended until type"
      _ -> typeCheckerErrorMsg file functionName "nowRule type ascription is not until type"

waitRule :: FilePath -> String -> Context -> [String] -> BExp -> BExp -> (CExp, BType)
waitRule file functionName context varStack e1 e2 =
  do
    let (e1Exp, e1Type) = mainTypeChecker file functionName context varStack e1
    let (e2Exp, e2Type) = mainTypeChecker file functionName context varStack e2
    case e2Type of
      BTypeAt (BTypeUntil a b) ->
        if generalBTypeCompare a e1Type
          then (CExpWait e1Exp e2Exp, BTypeUntil a b)
          else typeCheckerErrorMsg file functionName "waitRule two arguments do not match"
      _ -> typeCheckerErrorMsg file functionName "waitRule's second argument is not an at-delayed Until type"

urecRule :: FilePath -> String -> Context -> [String] -> BExp -> String -> BExp -> String -> String -> String -> BExp -> (CExp, BType)
urecRule file functionName context varStack e nowVar e1 waitVar1 waitVar2 fbyVar e2 = case context of
  TokenlessContext x0 -> typeCheckerErrorMsg file functionName "urecRule applied to a tokenless context"
  StableContext x0 x1 -> urecRuleHelper x0
  ArrowContext x0 x1 x2 -> urecRuleHelper x0
  AtContext x0 x1 x2 -> urecRuleHelper x0
  where
    urecRuleHelper firstContextList =
      do
        let (eExp, eType) = mainTypeChecker file functionName context varStack e
        case eType of
          BTypeUntil a b ->
            do
              let (e1Exp, e1Type) = mainTypeChecker file functionName (StableContext firstContextList [(nowVar, b, Nothing)]) (nowVar : varStack) e1
              let extendedContext = StableContext firstContextList [(fbyVar, BTypeAt e1Type, Nothing), (waitVar2, BTypeAt eType, Nothing), (waitVar1, a, Nothing)]
              let (e2Exp, e2Type) = mainTypeChecker file functionName extendedContext (fbyVar : waitVar2 : waitVar1 : varStack) e1
              if generalBTypeCompare e2Type e1Type
                then (CExpUrec eExp e1Exp e2Exp, e1Type)
                else typeCheckerErrorMsg file functionName "urecRule branches' types do not match"
          _ -> typeCheckerErrorMsg file functionName "urecRule not applied to an until type"

recRule :: FilePath -> String -> Context -> [String] -> String -> BType -> BExp -> (CExp, BType)
recRule file functionName context varStack var ascription e = case context of
  TokenlessContext x0 ->
    case ascription of
      BTypeBox a ->
        do
          let (eExp, eType) = mainTypeChecker file functionName (StableContext ((var, BTypeBox (BTypeArrow a), Nothing) : x0) []) (var : varStack) e
          if generalBTypeCompare eType a
            then (CExpRec eExp, ascription)
            else typeCheckerErrorMsg file functionName "recRule type ascription does not match target type"
      _ -> typeCheckerErrorMsg file functionName "recRule type ascription is not box type"
  StableContext x0 x1 -> typeCheckerErrorMsg file functionName "recRule not applied in a tokenlessContext, i.e. StableContext"
  ArrowContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "recRule not applied in a tokenlessContext, i.e. ArrowContext"
  AtContext x0 x1 x2 -> typeCheckerErrorMsg file functionName "recRule not applied in a tokenlessContext, i.e. AtContext"

outRule :: FilePath -> String -> Context -> [String] -> BExp -> (CExp, BType)
outRule file functionName context varStack e =
  do
    let (eExp, eType) = mainTypeChecker file functionName context varStack e
    case eType of
      BTypeFix t ->
        (CExpOut eExp, unfoldBType eType)
      _ -> typeCheckerErrorMsg file functionName "outRule not applied to an Fix type"

intoRule :: FilePath -> String -> Context -> [String] -> BExp -> BType -> (CExp, BType)
intoRule = error "not implemented"

typeCheckerErrorMsg :: FilePath -> String -> [Char] -> (CExp, BType)
typeCheckerErrorMsg file functionName msg =
  error (file ++ ": " ++ msg ++ " in \"" ++ functionName ++ "\"")

{-
import Datatype
import TypeChecker.ContextFunctions
import TypeChecker.LimitChecker
import TypeChecker.StableChecker
import TypeChecker.TypeFunctions

mainTypeChecker :: TypeCheckedProgram -> AExp -> Maybe AType
mainTypeChecker typedFunctions exp =
  declarationTypeChecker (TokenlessContext (removeExp typedFunctions)) exp
  where
    removeExp :: TypeCheckedProgram -> ContextElemList
    removeExp l =
      case l of
        [] -> []
        (var, _, t) : tl -> (var, t) : removeExp tl

declarationTypeChecker :: Context -> AExp -> Maybe AType
declarationTypeChecker context exp = case exp of
  AExpVar s -> aExpVarRule context s
  AExpUnit -> Just ATypeUnit
  AExpLambda s at ae -> aExpLambdaRule context s at ae
  AExpApplication ae ae' -> aExpApplicationRule context ae ae'
  AExpProduct ae ae' -> aExpProductRule context ae ae'
  AExpFst ae -> aExpFstRule context ae
  AExpSnd ae -> aExpSndRule context ae
  AExpInl ae at -> aExpInlRule context ae at
  AExpInr ae at -> aExpInrRule context ae at
  AExpMatch ae s ae' str ae2 -> aExpMatchRule context ae s ae' str ae2
  AExpZero -> return ATypeNat
  AExpSuc ae -> aExpSucRule context ae
  AExpPrimrec ae ae' s str ae2 -> aExpPrimrecRule context ae ae' s str ae2
  AExpArrow ae -> aExpArrowRule context ae
  AExpAt ae -> aExpAtRule context ae
  AExpAdv ae -> aExpAdvRule context ae
  AExpBox ae -> aExpBoxRule context ae
  AExpUnbox ae -> aExpUnboxRule context ae
  AExpNow ae at -> aExpNowRule context ae at
  AExpWait ae ae' -> aExpWaitRule context ae ae'
  AExpUrec ae s ae' str cs s' ae2 -> aExpUrecRule context ae s ae' str cs s' ae2
  AExpFix s at ae -> aExpFixRule context s at ae
  AExpOut ae -> aExpOutRule context ae
  AExpInto ae at -> aExpIntoRule context ae at
  _ -> Nothing

aExpVarRule :: Context -> String -> Maybe AType
aExpVarRule context s = case context of
  TokenlessContext x0 -> elemContext x0 s
  StableContext x0 x1 -> case elemContext x1 s of
    Just at -> Just at
    Nothing -> case elemContext x0 s of
      Nothing -> Nothing
      Just at -> if isStable at then Just at else Nothing
  ArrowContext x0 x1 x2 -> case elemContext x2 s of
    Just at -> Just at
    Nothing -> case elemContext (x1 ++ x0) s of
      Nothing -> Nothing
      Just at -> if isStable at then Just at else Nothing
  AtContext x0 x1 x2 -> case elemContext x2 s of
    Just at -> Just at
    Nothing -> case elemContext (x1 ++ x0) s of
      Nothing -> Nothing
      Just at -> if isStable at then Just at else Nothing

aExpLambdaRule :: Context -> String -> AType -> AExp -> Maybe AType
aExpLambdaRule c var t exp =
  if isTickFree c
    then case declarationTypeChecker
      (addContextElem c (var, t))
      exp of
      Nothing -> Nothing
      Just at -> Just (ATypeFunction t at)
    else Nothing

aExpApplicationRule :: Context -> AExp -> AExp -> Maybe AType
aExpApplicationRule c e1 e2 =
  do
    t1 <- declarationTypeChecker c e1
    t2 <- declarationTypeChecker c e2
    case t1 of
      ATypeFunction a b ->
        if areEqualTypes t2 a then return b else Nothing
      _ -> Nothing

aExpProductRule :: Context -> AExp -> AExp -> Maybe AType
aExpProductRule c exp1 exp2 = case declarationTypeChecker c exp1 of
  Nothing -> Nothing
  Just at -> case declarationTypeChecker c exp2 of
    Nothing -> Nothing
    Just at' -> return (ATypeProduct at at')

aExpFstRule :: Context -> AExp -> Maybe AType
aExpFstRule c exp = case declarationTypeChecker c exp of
  Nothing -> Nothing
  Just at -> case at of
    ATypeProduct t1 t2 -> return t1
    _ -> Nothing

aExpSndRule :: Context -> AExp -> Maybe AType
aExpSndRule c exp = case declarationTypeChecker c exp of
  Nothing -> Nothing
  Just at -> case at of
    ATypeProduct t1 t2 -> return t2
    _ -> Nothing

aExpInlRule :: Context -> AExp -> AType -> Maybe AType
aExpInlRule context exp t = case t of
  ATypeSum t1 t2 ->
    case declarationTypeChecker
      context
      exp of
      Nothing -> Nothing
      Just t3 -> if areEqualTypes t3 t1 then return (ATypeSum t1 t2) else Nothing
  _ -> Nothing

aExpInrRule :: Context -> AExp -> AType -> Maybe AType
aExpInrRule context exp t = case t of
  ATypeSum t1 t2 ->
    case declarationTypeChecker context exp of
      Nothing -> Nothing
      Just t3 -> if areEqualTypes t3 t2 then return (ATypeSum t1 t2) else Nothing
  _ -> Nothing

aExpMatchRule :: Context -> AExp -> String -> AExp -> String -> AExp -> Maybe AType
aExpMatchRule c exp var1 exp1 var2 exp2 = case declarationTypeChecker c exp of
  Just (ATypeSum a1 a2) -> do
    b1 <- declarationTypeChecker (addContextElem c (var1, a1)) exp1
    b2 <- declarationTypeChecker (addContextElem c (var2, a2)) exp2
    if areEqualTypes b1 b2 then return b1 else Nothing
  _ -> Nothing

aExpSucRule :: Context -> AExp -> Maybe AType
aExpSucRule c exp =
  do
    t <- declarationTypeChecker c exp
    case t of
      ATypeNat -> return ATypeNat
      _ -> Nothing

aExpPrimrecRule :: Context -> AExp -> AExp -> String -> String -> AExp -> Maybe AType
aExpPrimrecRule c exp exp1 var1 var2 exp2 =
  do
    t1 <- declarationTypeChecker c exp
    case t1 of
      ATypeNat -> do
        t2 <- declarationTypeChecker c exp1
        t3 <- declarationTypeChecker (addContextElem (addContextElem c (var1, ATypeNat)) (var2, t2)) exp2
        if areEqualTypes t2 t3 then return t2 else Nothing
      _ -> Nothing

aExpArrowRule :: Context -> AExp -> Maybe AType
aExpArrowRule c exp =
  case c of
    StableContext l1 l2 -> do
      t <- declarationTypeChecker (ArrowContext l1 l2 []) exp
      return (ATypeArrow t)
    _ -> Nothing

aExpAtRule :: Context -> AExp -> Maybe AType
aExpAtRule c exp =
  case c of
    StableContext l1 l2 -> do
      t <- declarationTypeChecker (AtContext l1 l2 []) exp
      return (ATypeAt t)
    _ -> Nothing

aExpAdvRule :: Context -> AExp -> Maybe AType
aExpAdvRule c exp =
  case c of
    AtContext l1 l2 l3 -> do
      t <- declarationTypeChecker (StableContext l1 l2) exp
      case t of
        ATypeAt t' -> return t'
        ATypeArrow t' -> if isLimit t' then return t' else Nothing
        _ -> Nothing
    ArrowContext l1 l2 l3 -> do
      t <- declarationTypeChecker (StableContext l1 l2) exp
      case t of
        ATypeAt t' -> return t'
        ATypeArrow t' -> return t'
        _ -> Nothing
    _ -> Nothing

aExpBoxRule :: Context -> AExp -> Maybe AType
aExpBoxRule c exp =
  case c of
    TokenlessContext l ->
      do
        a <- declarationTypeChecker (StableContext l []) exp
        return (ATypeBox a)
    _ -> Nothing

aExpUnboxRule :: Context -> AExp -> Maybe AType
aExpUnboxRule c exp =
  case c of
    TokenlessContext l -> Nothing
    StableContext l1 l2 -> do
      a <- declarationTypeChecker (TokenlessContext l1) exp
      case a of
        ATypeBox a' -> return a'
        _ -> Nothing
    AtContext l1 l2 l3 -> do
      a <- declarationTypeChecker (TokenlessContext l1) exp
      case a of
        ATypeBox a' -> return a'
        _ -> Nothing
    ArrowContext l1 l2 l3 -> do
      a <- declarationTypeChecker (TokenlessContext l1) exp
      case a of
        ATypeBox a' -> return a'
        _ -> Nothing

aExpNowRule :: Context -> AExp -> AType -> Maybe AType
aExpNowRule c exp t =
  case t of
    ATypeUntil a b ->
      do
        b' <- declarationTypeChecker c exp
        if areEqualTypes b b' then return t else Nothing
    _ -> Nothing

aExpWaitRule :: Context -> AExp -> AExp -> Maybe AType
aExpWaitRule c exp1 exp2 =
  do
    x <- declarationTypeChecker c exp1
    y <- declarationTypeChecker c exp2
    case y of
      ATypeAt (ATypeUntil a b) ->
        if areEqualTypes a y then return (ATypeUntil a b) else Nothing
      _ -> Nothing

aExpUrecRule :: Context -> AExp -> String -> AExp -> [Char] -> [Char] -> [Char] -> AExp -> Maybe AType
aExpUrecRule c exp var1 exp1 var2 var3 var4 exp2 =
  case c of
    TokenlessContext l -> Nothing
    StableContext l1 l2 ->
      do
        aUb <- declarationTypeChecker c exp
        case aUb of
          ATypeUntil a b ->
            do
              let newcontext = StableContext l1 []
              c <- declarationTypeChecker (addContextElem (newcontext) (var1, b)) exp1
              let newcontext' = addContextElem (addContextElem (addContextElem newcontext (var2, a)) (var3, ATypeAt (aUb))) (var4, ATypeAt c)
              c' <- declarationTypeChecker newcontext' exp2
              if areEqualTypes c c' then return c else Nothing
          _ -> Nothing
    AtContext l1 l2 l3 ->
      do
        aUb <- declarationTypeChecker c exp
        case aUb of
          ATypeUntil a b ->
            do
              let newcontext = StableContext l1 []
              c <- declarationTypeChecker (addContextElem (newcontext) (var1, b)) exp1
              let newcontext' = addContextElem (addContextElem (addContextElem newcontext (var2, a)) (var3, ATypeAt (aUb))) (var4, ATypeAt c)
              c' <- declarationTypeChecker newcontext' exp2
              if areEqualTypes c c' then return c else Nothing
          _ -> Nothing
    ArrowContext l1 l2 l3 ->
      do
        aUb <- declarationTypeChecker c exp
        case aUb of
          ATypeUntil a b ->
            do
              let newcontext = StableContext l1 []
              c <- declarationTypeChecker (addContextElem (newcontext) (var1, b)) exp1
              let newcontext' = addContextElem (addContextElem (addContextElem newcontext (var2, a)) (var3, ATypeAt (aUb))) (var4, ATypeAt c)
              c' <- declarationTypeChecker newcontext' exp2
              if areEqualTypes c c' then return c else Nothing
          _ -> Nothing

aExpFixRule :: Context -> String -> AType -> AExp -> Maybe AType
aExpFixRule c var t exp =
  case c of
    TokenlessContext l ->
      case t of
        ATypeBox a ->
          do
            a' <- declarationTypeChecker (StableContext ((var, ATypeBox (ATypeArrow a)) : l) []) exp
            if areEqualTypes a' a then return (ATypeBox a) else Nothing
        _ -> Nothing
    _ -> Nothing

aExpOutRule :: Context -> AExp -> Maybe AType
aExpOutRule c exp =
  do
    t <- declarationTypeChecker c exp
    case t of
      ATypeFix a a' ->
        fixUnfold t
      _ -> Nothing

aExpIntoRule :: Context -> AExp -> AType -> Maybe AType
aExpIntoRule c exp t =
  case t of
    ATypeFix a a' ->
      do
        target <- declarationTypeChecker c exp
        t' <- fixUnfold t
        if areEqualTypes target t' then return t else Nothing
    _ -> Nothing

-}