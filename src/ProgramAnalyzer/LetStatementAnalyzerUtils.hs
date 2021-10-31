module ProgramAnalyzer.LetStatementAnalyzerUtils where

import Datatype

checkFunctionNameExists :: TypeCheckedProgram -> String -> IO ()
checkFunctionNameExists createdFunctionNames functionName = case createdFunctionNames of
  [] -> return ()
  (str, _, _, _) : tl -> if str == functionName then error (functionName ++ " already defined or exported!") else checkFunctionNameExists tl functionName