module ProgramAnalyzer.TypeStatementAnalyzerUtils where

import Datatype

checkTypeSynonymNameExists :: FilePath -> TypenameList -> String -> IO ()
checkTypeSynonymNameExists file createdTypeSynonymNames typeSynonymName = case createdTypeSynonymNames of
  [] -> return ()
  (name, _, _) : tl ->
    if typeSynonymName == name
      then error (file ++ ": Type Synonym \"" ++ typeSynonymName ++ "\" already defined or exported!")
      else checkTypeSynonymNameExists file tl typeSynonymName