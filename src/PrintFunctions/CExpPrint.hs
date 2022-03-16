module PrintFunctions.CExpPrint where

import Datatype

printCExp :: Integer -> CExp -> String
printCExp n cExp =
  do
    let currentLevel = cExpLevel cExp
    case cExp of
      CExpIndex i -> "\'v" ++ show (n -1 - i)
      CExpUnit -> "()"
      CExpLambda ce ->
        "fun " ++ "\'v" ++ show n ++ " => "
          ++ printCExp (n + 1) ce
      CExpApplication ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ " "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpProduct ce ce' ->
        "("
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
          ++ ", "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
          ++ ")"
      CExpFst ce ->
        "fst "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpSnd ce ->
        "snd "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpInl ce ->
        "inl "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpInr ce ->
        "inr "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpMatch ce ce' ce2 ->
        "match "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
          ++ " with | inl "
          ++ "\'v"
          ++ show n
          ++ " => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp (n + 1) ce' ++ ")"
                 else printCExp (n + 1) ce'
             )
          ++ " | inl "
          ++ "\'v"
          ++ show n
          ++ " => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp (n + 1) ce2 ++ ")"
                 else printCExp (n + 1) ce2
             )
      CExpZero -> "0"
      CExpSuc ce b -> do
        let maybeN = allSuc ce
        case maybeN of
          Nothing ->
            "suc "
              ++ ( if cExpLevel ce <= cExpLevel cExp
                     then "(" ++ printCExp n ce ++ ")"
                     else printCExp n ce
                 )
          Just n -> show (n + 1)
        where
          allSuc :: CExp -> Maybe Integer
          allSuc cExp = case cExp of
            CExpZero -> return 0
            CExpSuc c b -> do
              rest <- allSuc c
              return (rest + 1)
            _ -> Nothing
      CExpPrimrec ce ce' ce2 ->
        "primrec "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
          ++ " with | 0 => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
          ++ " | suc "
          ++ "\'v"
          ++ show n
          ++ ", "
          ++ "\'v"
          ++ show (n + 1)
          ++ " => "
          ++ ( if cExpLevel ce2 <= cExpLevel cExp
                 then "(" ++ printCExp (n + 2) ce2 ++ ")"
                 else printCExp (n + 2) ce2
             )
      CExpAdv ce ->
        "< "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpDelay ce ->
        "> "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpBox ce ->
        "# "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpUnbox ce ->
        "? "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpNow ce ->
        "now "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpWait ce ce' ->
        "wait "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
          ++ " "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpUrec ce ce' ce2 ->
        "urec "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
          ++ " with | now "
          ++ "\'v"
          ++ show n
          ++ " => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp (n + 1) ce' ++ ")"
                 else printCExp (n + 1) ce'
             )
          ++ " | wait "
          ++ "\'v"
          ++ show n
          ++ " \'v"
          ++ show (n + 1)
          ++ ", "
          ++ "\'v"
          ++ show (n + 2)
          ++ " => "
          ++ ( if cExpLevel ce2 <= cExpLevel cExp
                 then "(" ++ printCExp (n + 3) ce2 ++ ")"
                 else printCExp (n + 3) ce2
             )
      CExpNfix ce ->
        "nfix " ++ "\'v" ++ show n ++ " => "
          ++ printCExp (n + 1) ce
      CExpOut ce ->
        "out "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpInto ce ->
        "into "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpLocation i -> "l_" ++ show (i)
      CExpTrue -> "true"
      CExpFalse -> "false"
      CExpIf ce ce' ce2 ->
        "if " ++ printCExp n ce ++ " then " ++ printCExp n ce' ++ " else "
          ++ ( if cExpLevel ce2 <= cExpLevel cExp
                 then "(" ++ printCExp n ce2 ++ ")"
                 else printCExp n ce2
             )
      CExpAnd ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ " and "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpOr ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ " or "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpNot ce ->
        "not "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpEquals ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "=="
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpNotEquals ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "!="
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpInteger n -> show (n)
      CExpIncrement ce -> do
        "suc "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
      CExpAdd ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "+"
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpMinus ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "-"
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpMultiply ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "*"
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpDivide ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "/"
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpMod ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "%"
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpPower ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "^"
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpList ces b -> "[" ++ cExpListToString n ces ++ "]"
      CExpListAppend ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "++"
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpListCons ce ce' ->
        ( if cExpLevel ce <= cExpLevel cExp
            then "(" ++ printCExp n ce ++ ")"
            else printCExp n ce
        )
          ++ "::"
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
      CExpListRec ce ce' ce2 ->
        "primrec "
          ++ ( if cExpLevel ce <= cExpLevel cExp
                 then "(" ++ printCExp n ce ++ ")"
                 else printCExp n ce
             )
          ++ " with | [] => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp n ce' ++ ")"
                 else printCExp n ce'
             )
          ++ " | "
          ++ "\'v"
          ++ show n
          ++ "::"
          ++ "\'v"
          ++ show (n + 1)
          ++ ", "
          ++ "\'v"
          ++ show (n + 2)
          ++ " => "
          ++ ( if cExpLevel ce2 <= cExpLevel cExp
                 then "(" ++ printCExp (n + 3) ce2 ++ ")"
                 else printCExp (n + 2) ce2
             )

--Probably all wrong
cExpLevel :: CExp -> Integer
cExpLevel cExp = case cExp of
  CExpIndex n -> 11
  CExpUnit -> 11
  CExpLambda ce -> 0
  CExpApplication ce ce' -> 0
  CExpProduct ce ce' -> 11
  CExpFst ce -> 10
  CExpSnd ce -> 10
  CExpInl ce -> 9
  CExpInr ce -> 9
  CExpMatch ce ce' ce2 -> 0
  CExpZero -> 11
  CExpSuc ce b -> 10
  CExpPrimrec ce ce' ce2 -> 0
  CExpAdv ce -> 10
  CExpDelay ce -> 10
  CExpBox ce -> 10
  CExpUnbox ce -> 10
  CExpNow ce -> 10
  CExpWait ce ce' -> 9
  CExpUrec ce ce' ce2 -> 0
  CExpNfix ce -> 0
  CExpOut ce -> 10
  CExpInto ce -> 10
  CExpLocation n -> 11
  CExpTrue -> 11
  CExpFalse -> 11
  CExpIf ce ce' ce2 -> 0
  CExpAnd ce ce' -> 2
  CExpOr ce ce' -> 1
  CExpNot ce -> 3
  CExpEquals ce ce' -> 4
  CExpNotEquals ce ce' -> 4
  CExpInteger n -> 11
  CExpIncrement ce -> 10
  CExpAdd ce ce' -> 6
  CExpMinus ce ce' -> 6
  CExpMultiply ce ce' -> 7
  CExpDivide ce ce' -> 7
  CExpMod ce ce' -> 7
  CExpPower ce ce' -> 8
  CExpList ces b -> 11
  CExpListAppend ce ce' -> 5
  CExpListCons ce ce' -> 5
  CExpListRec ce ce' ce2 -> 0

cExpListToString :: Integer -> [CExp] -> String
cExpListToString n xs =
  case xs of
    [] -> ""
    x1 : x2 : xs' -> printCExp n x1 ++ ", " ++ cExpListToString n (x2 : xs')
    x : [] -> printCExp n x