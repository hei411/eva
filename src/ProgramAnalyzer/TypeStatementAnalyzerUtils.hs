module ProgramAnalyzer.TypeStatementAnalyzerUtils where

import Datatype

checkTypeSynonymNameExists :: FilePath -> TypenameList -> String -> IO ()
checkTypeSynonymNameExists file createdTypeSynonymNames typeSynonymName = case createdTypeSynonymNames of
  [] -> return ()
  (name, _, _) : tl ->
    if typeSynonymName == name
      then error (file ++ ": Type Synonym \"" ++ typeSynonymName ++ "\" already defined or exported!")
      else checkTypeSynonymNameExists file tl typeSynonymName

checkAliasClash :: FilePath -> [String] -> String -> IO ()
checkAliasClash file usedAlias typeSynonymName = case usedAlias of
  [] -> return ()
  s : ss ->
    if typeSynonymName == s
      then error (file ++ ": Type Synonym \"" ++ typeSynonymName ++ "\" already used as an alias for a qualified import!")
      else checkAliasClash file ss typeSynonymName