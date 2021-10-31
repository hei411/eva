module ProgramAnalyzer.LetStatementAnalyzerUtils where

import Datatype

checkFunctionNameExists :: FilePath -> TypeCheckedProgram -> String -> IO ()
checkFunctionNameExists file createdFunctionNames functionName = case createdFunctionNames of
  [] -> return ()
  (str, _, _, _) : tl -> if str == functionName then error (file ++ ": \"" ++ functionName ++ "\" already defined or exported!") else checkFunctionNameExists file tl functionName