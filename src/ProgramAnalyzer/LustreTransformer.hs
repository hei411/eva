module ProgramAnalyzer.LustreTransformer where

import Data.List
import Datatype
import Datatype (Program)

lustreTransform :: Program -> Program
lustreTransform p = case p of
  [] -> []
  state : states -> case state of
    LustreStatement s x0 x1 ld -> lustreNodeTransform s x0 x1 ld : lustreTransform states
    _ -> state : lustreTransform states

lustreNodeTransform :: String -> [(String, LustreType)] -> (String, LustreType) -> LustreDef -> Statement
lustreNodeTransform name args returnThing definition =
  do
    let (returnVar, returnType) = returnThing
    case definition of
      LustreSimpleExp s le ->
        if s == returnVar
          then do
            let helper = generateHelper args returnThing le le name
            let body = generateBody args returnThing
            ((DefStatement name [] (AExpLet "'helper" helper body)))
          else error ("return variable does not match definition variable in node " ++ name)
      LustreArrowExp s le le' ->
        if s == returnVar
          then do
            let helper = generateHelper args returnThing le le' name
            let body = generateBody args returnThing
            ((DefStatement name [] (AExpLet "'helper" helper body)))
          else error ("return variable does not match definition variable in node " ++ name)

generateHelper :: [(String, LustreType)] -> (String, LustreType) -> LustreExp -> LustreExp -> String -> AExp
generateHelper args returnThing head tail name = do
  let helperType = createHelperType (map snd args) (snd returnThing)
  AExpNfix "'helper" helperType (generateHelperBody args returnThing head tail name)

generateHelperBody :: [(String, LustreType)] -> (String, LustreType) -> LustreExp -> LustreExp -> String -> AExp
generateHelperBody args returnThing hd tl name =
  do
    let case1 = generateCase (map fst args) (fst returnThing) True name hd
    let continuation1 = generateContinuation case1 (toInteger (length args))
    let case2 = generateCase (map fst args) (fst returnThing) False name tl
    let continuation2 = generateContinuation case2 (toInteger (length args))
    let body = AExpIf (AExpVar "'b" []) (AExpStreamCons case1 continuation1) (AExpStreamCons case2 continuation2)
    let withLetBody = generateLet 0 (toInteger (length args)) body
    let halfLambda = AExpLambda "'preReturn" (changeType (snd returnThing)) (AExpLambda "'b" ATypeBool (createRight args withLetBody 0))
    createLeft args halfLambda 0
  where
    createRight :: [(String, LustreType)] -> AExp -> Integer -> AExp
    createRight l rest num = case l of
      [] -> rest
      x0 : x1 -> AExpLambda ("'str" ++ (show num)) (changeStreamType (snd x0)) (createRight x1 rest (num + 1))
    createLeft :: [(String, LustreType)] -> AExp -> Integer -> AExp
    createLeft l rest num = case l of
      [] -> rest
      x0 : x1 -> AExpLambda ("'pre" ++ (show num)) (changeType (snd x0)) (createLeft x1 rest (num + 1))
    generateLet :: Integer -> Integer -> AExp -> AExp
    generateLet now target exp =
      if now == target
        then exp
        else AExpLetStream ("'cur" ++ show now) ("'next" ++ show now) (AExpVar ("'str" ++ show now) []) (generateLet (1 + now) target exp)

generateContinuation :: AExp -> Integer -> AExp
generateContinuation c len =
  do
    let l = [AExpAdv (AExpUnbox (AExpVar "'helper" []))]
    let l' = l ++ (addCur 0 len) ++ [c, AExpFalse] ++ (addNext 0 len)
    AExpAngle (folder l')
  where
    addCur :: Integer -> Integer -> [AExp]
    addCur now target =
      if now == target
        then []
        else [AExpVar ("'cur" ++ show now) []] ++ (addCur (now + 1) target)
    addNext :: Integer -> Integer -> [AExp]
    addNext now target =
      if now == target
        then []
        else [AExpAdv (AExpVar ("'next" ++ show now) [])] ++ (addNext (now + 1) target)
    folder :: [AExp] -> AExp
    folder l = case l of
      [] -> error "folder error, should not happen"
      ae : [] -> ae
      ae : ae' : rest -> folder ((AExpApplication ae ae') : rest)

generateCase :: [(String)] -> (String) -> Bool -> String -> LustreExp -> AExp
generateCase args returnThing isCase1 name exp = case exp of
  LustreExpInt n -> AExpInteger n
  LustreExpVar s ->
    if s == returnThing
      then error ("Cannot reference the same return Stream at the same time step in node " ++ name)
      else findCur args s name 0
  LustreExpPre s ->
    if isCase1
      then error ("Cannot reference the previous element of " ++ s ++ " for the first element in node " ++ name)
      else
        if s == returnThing
          then AExpVar "'preReturn" []
          else findPre args s name 0
  LustreExpTrue -> AExpTrue
  LustreExpFalse -> AExpFalse
  LustreExpIf le le' le2 -> AExpIf (h le) (h le') (h le2)
  LustreExpAnd le le' -> AExpAnd (h le) (h le')
  LustreExpOr le le' -> AExpOr (h le) (h le')
  LustreExpNot le -> AExpNot (h le)
  LustreExpAdd le le' -> AExpAdd (h le) (h le')
  LustreExpMinus le le' -> AExpMinus (h le) (h le')
  LustreExpMultiply le le' -> AExpMultiply (h le) (h le')
  LustreExpDivide le le' -> AExpDivide (h le) (h le')
  LustreExpEquals le le' -> AExpEquals (h le) (h le')
  LustreExpSmaller le le' -> AExpNot (AExpEquals (AExpInteger 0) (AExpMinus (h le') (h le)))
  LustreExpLarger le le' -> AExpNot (AExpEquals (AExpInteger 0) (AExpMinus (h le) (h le')))
  where
    h = generateCase args returnThing isCase1 name
    findCur :: [String] -> String -> String -> Integer -> AExp
    findCur l target nodeName num = case l of
      [] -> error ("Cannot find variable " ++ target ++ " in node " ++ nodeName)
      s : ss -> if s == target then AExpVar ("'cur" ++ (show num)) [] else findCur ss target nodeName (num + 1)
    findPre :: [String] -> String -> String -> Integer -> AExp
    findPre l target nodeName num = case l of
      [] -> error ("Cannot find variable " ++ target ++ " in node " ++ nodeName)
      s : ss -> if s == target then AExpVar ("'pre" ++ (show num)) [] else findPre ss target nodeName (num + 1)

createHelperType :: [LustreType] -> LustreType -> AType
createHelperType argsType returnType = do
  let r = ATypeFunction (changeType returnType) (ATypeFunction ATypeBool (createRight argsType returnType))
  ATypeBox (createWhole argsType r)
  where
    createRight :: [LustreType] -> LustreType -> AType
    createRight argsType returnType = case argsType of
      [] -> changeStreamType returnType
      lt : lts -> ATypeFunction (changeStreamType lt) (createRight lts returnType)
    createWhole :: [LustreType] -> AType -> AType
    createWhole l rest = case l of
      [] -> rest
      lt : lts -> ATypeFunction (changeType lt) (createWhole lts rest)

generateBody :: [(String, LustreType)] -> (String, LustreType) -> AExp
generateBody args returnThing =
  do
    let l = [AExpUnbox (AExpVar "'helper" [])]
    let l' = l ++ map defaultVal (map snd args) ++ [defaultVal (snd returnThing), AExpTrue]
    AExpBox (folder l')
  where
    defaultVal :: LustreType -> AExp
    defaultVal t = case t of
      LustreBool -> AExpTrue
      LustreInt -> AExpInteger 0
    folder :: [AExp] -> AExp
    folder l = case l of
      [] -> error "folder error, should not happen"
      ae : [] -> ae
      ae : ae' : rest -> folder ((AExpApplication ae ae') : rest)

changeType :: LustreType -> AType
changeType t = case t of
  LustreBool -> ATypeBool
  LustreInt -> ATypeNat

changeStreamType :: LustreType -> AType
changeStreamType t = case t of
  LustreBool -> (ATypeNFix "'s" (ATypeProduct ATypeBool (ATypeVar "'s")))
  LustreInt -> (ATypeNFix "'s" (ATypeProduct ATypeNat (ATypeVar "'s")))
