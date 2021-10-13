module TypeChecker.ValidChecker where

import Datatype

isValidProgramMain :: Program -> IO ()
isValidProgramMain program =
  ( do
      let allValidTypes = isValidProgram program
      case allValidTypes of
        Right (s, t) -> fail (s ++ " has invalid type ascription: " ++ show (t) ++ "!")
        Left _ -> return ()
  )

isValidProgram :: Program -> Either () (String, AType)
isValidProgram l = case l of
  [] -> Left ()
  hd : tl -> case hd of
    LetStatement s exp -> case isValidExp exp of
      Left _ -> isValidProgram tl
      Right t -> Right (s, t)

isValidExp :: AExp -> Either () AType
isValidExp e = case e of
  AExpLambda s t exp -> if not (isValidType t) then Right t else isValidExp exp
  AExpApplication exp1 exp2 -> case isValidExp exp1 of
    Left _ -> isValidExp exp2
    x -> x
  AExpProduct exp1 exp2 -> case isValidExp exp1 of
    Left _ -> isValidExp exp2
    x -> x
  AExpFst exp -> isValidExp exp
  AExpSnd exp -> isValidExp exp
  AExpInl exp t -> if not (isValidType t) then Right t else isValidExp exp
  AExpInr exp t -> if not (isValidType t) then Right t else isValidExp exp
  AExpMatch exp1 s1 exp2 s2 exp3 -> case isValidExp exp1 of
    Left _ -> case isValidExp exp2 of
      Left _ -> isValidExp exp3
      x -> x
    x -> x
  AExpSuc exp -> isValidExp exp
  AExpPrimrec exp1 exp2 s1 s2 exp3 -> case isValidExp exp1 of
    Left _ -> case isValidExp exp2 of
      Left _ -> isValidExp exp3
      x -> x
    x -> x
  AExpArrow exp -> isValidExp exp
  AExpAt exp -> isValidExp exp
  AExpAdv exp -> isValidExp exp
  AExpBox exp -> isValidExp exp
  AExpUnbox exp -> isValidExp exp
  AExpNow exp t -> if not (isValidType t) then Right t else isValidExp exp
  AExpWait exp1 exp2 -> case isValidExp exp1 of
    Left _ -> isValidExp exp2
    x -> x
  AExpUrec exp1 s1 exp2 s2 s3 s4 exp3 -> case isValidExp exp1 of
    Left _ -> case isValidExp exp2 of
      Left _ -> isValidExp exp3
      x -> x
    x -> x
  AExpFix s t exp -> if not (isValidType t) then Right t else isValidExp exp
  AExpOut exp -> isValidExp exp
  AExpInto exp t -> if not (isValidType t) then Right t else isValidExp exp
  _ -> Left ()

isValidType :: AType -> Bool
isValidType t = isValidHelper t []
  where
    isValidHelper t l = case t of
      ATypeVar s -> elem s l
      ATypeFix s exp -> isValidHelper exp (s : l)
      ATypeProduct x y -> (isValidHelper x l) && (isValidHelper y l)
      ATypeSum x y -> (isValidHelper x l) && (isValidHelper y l)
      ATypeFunction x y -> (isValidHelper x l) && (isValidHelper y l)
      ATypeUntil x y -> (isValidHelper x l) && (isValidHelper y l)
      ATypeBox x -> (isValidHelper x l)
      ATypeArrow x -> (isValidHelper x l)
      ATypeAt x -> (isValidHelper x l)
      _ -> True
