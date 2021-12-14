module PrintFunctions.CExpPrint where

import Datatype

printCExp :: Integer -> CExp -> String
printCExp n cExp =
  do
    let currentLevel = cExpLevel cExp
    case cExp of
      CExpIndex i -> "\'" ++ show (n -1 - i)
      CExpUnit -> "()"
      CExpLambda ce ->
        "fun " ++ "\'" ++ show n ++ " => "
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
          ++ "\'"
          ++ show n
          ++ " => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp (n + 1) ce' ++ ")"
                 else printCExp (n + 1) ce'
             )
          ++ " | inl "
          ++ "\'"
          ++ show n
          ++ " => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp (n + 1) ce2 ++ ")"
                 else printCExp (n + 1) ce2
             )
      CExpZero -> "0"
      CExpSuc ce -> do
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
            CExpSuc c -> do
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
          ++ "\'"
          ++ show n
          ++ ", "
          ++ "\'"
          ++ show (n + 1)
          ++ " => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
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
          ++ "\'"
          ++ show n
          ++ " => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp (n + 1) ce' ++ ")"
                 else printCExp (n + 1) ce'
             )
          ++ " | wait "
          ++ "\'"
          ++ show n
          ++ " \'"
          ++ show (n + 1)
          ++ ", "
          ++ "\'"
          ++ show (n + 2)
          ++ " => "
          ++ ( if cExpLevel ce' <= cExpLevel cExp
                 then "(" ++ printCExp (n + 3) ce2 ++ ")"
                 else printCExp (n + 3) ce2
             )
      CExpRec ce ->
        "rec " ++ "\'" ++ show n ++ " => "
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

cExpLevel :: CExp -> Integer
cExpLevel cExp = case cExp of
  CExpIndex n -> 3
  CExpUnit -> 3
  CExpLambda ce -> -3
  CExpApplication ce ce' -> 1
  CExpProduct ce ce' -> 2
  CExpFst ce -> 2
  CExpSnd ce -> 2
  CExpInl ce -> 2
  CExpInr ce -> 2
  CExpMatch ce ce' ce2 -> 2
  CExpZero -> 3
  CExpSuc ce -> 2
  CExpPrimrec ce ce' ce2 -> 2
  CExpAdv ce -> 2
  CExpDelay ce -> 2
  CExpBox ce -> 2
  CExpUnbox ce -> 2
  CExpNow ce -> 2
  CExpWait ce ce' -> 2
  CExpUrec ce ce' ce2 -> 2
  CExpRec ce -> -3
  CExpOut ce -> 2
  CExpInto ce -> 2
  CExpLocation n -> 3
  CExpTrue -> 3
  CExpFalse -> 3
  CExpIf ce ce' ce2 -> 1
  CExpAnd ce ce' -> 1
  CExpOr ce ce' -> 1
  CExpNot ce -> 2
  CExpEquals ce ce' -> 1
  CExpNotEquals ce ce' -> 1
  CExpInteger n -> 3
  CExpIncrement ce -> 2
  CExpAdd ce ce' -> -2
  CExpMinus ce ce' -> -2
  CExpMultiply ce ce' -> -1
  CExpDivide ce ce' -> -1
  CExpMod ce ce' -> -1
  CExpPower ce ce' -> 0