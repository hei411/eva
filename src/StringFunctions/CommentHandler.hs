module StringFunctions.CommentHandler where

commentRemover :: String -> String
commentRemover str = case str of
  [] -> []
  [c] -> [c]
  '/' : '/' : tl -> "  " ++ findLn tl
  '/' : '*' : tl -> "  " ++ findClose tl
  hd : tl -> hd : (commentRemover tl)

findLn :: String -> String
findLn str = case str of
  [] -> []
  '\n' : tl -> '\n' : commentRemover tl
  hd : tl -> ' ' : findLn tl

findClose :: String -> String
findClose str = case str of
  [] -> []
  [a] -> [' ']
  '*' : '/' : tl -> "  " ++ commentRemover tl
  hd : tl -> if hd == '\n' then '\n' : findClose tl else ' ' : findClose tl