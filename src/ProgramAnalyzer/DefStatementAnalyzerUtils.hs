module ProgramAnalyzer.DefStatementAnalyzerUtils where

import Datatype

checkFunctionNameExists :: FilePath -> TypeCheckedProgram -> String -> IO ()
checkFunctionNameExists file createdFunctionNames functionName = case createdFunctionNames of
  [] -> return ()
  (str, _, _, _) : tl -> if str == functionName then error (file ++ ": Function \"" ++ functionName ++ "\" already defined or imported!") else checkFunctionNameExists file tl functionName