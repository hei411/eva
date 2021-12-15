module ProgramAnalyzer.ImportStatementAnalyzerUtils where

import Datatype

checkCircularDependency :: FilePath -> FilePath -> [FilePath] -> IO ()
checkCircularDependency toImportFile currentFile toCompileFiles =
  if elem toImportFile toCompileFiles
    then error (currentFile ++ ": Cannot analyze \"" ++ toImportFile ++ "\" due to circular dependency!")
    else return ()

findFileData :: FilePath -> CompiledFilesData -> Maybe (TypeCheckedProgram, TypenameList)
findFileData toImportFile compiledFilesData = case compiledFilesData of
  [] -> Nothing
  (file, functions, typeSynonyms) : tl ->
    if file == toImportFile
      then return (functions, typeSynonyms)
      else findFileData toImportFile tl

addFunctions :: [Char] -> FilePath -> String -> TypeCheckedProgram -> TypeCheckedProgram -> TypeCheckedProgram -> TypeCheckedProgram
addFunctions currentFile toImportFile alias toImportFunctions importedFunctions toExportFunctions =
  case toImportFunctions of
    [] -> importedFunctions
    (name, cExp, bType, typeProperties) : tl ->
      do
        let result = checkFunctionNameExists (alias ++ name) (importedFunctions ++ toExportFunctions)
        --seq needed to force result
        result `seq` addFunctions currentFile toImportFile alias tl (((alias ++ name), cExp, bType, typeProperties) : importedFunctions) toExportFunctions
      where
        checkFunctionNameExists :: String -> TypeCheckedProgram -> ()
        checkFunctionNameExists name functionList = case functionList of
          [] -> ()
          (otherName, _, _, _) : tl ->
            if otherName == name
              then error (currentFile ++ ": Found same function name \"" ++ name ++ "\" imported or declared when importing " ++ toImportFile)
              else checkFunctionNameExists name tl

addTypenames :: [Char] -> FilePath -> String -> TypenameList -> TypenameList -> TypenameList -> TypenameList
addTypenames currentFile toImportFile alias toImportTypenames importedTypenames toExportTypenames =
  case toImportTypenames of
    [] -> importedTypenames
    (name, bType, num) : tl ->
      do
        let _ = checkTypeNameExists (alias ++ name) (importedTypenames ++ toExportTypenames)
        addTypenames currentFile toImportFile alias tl (((alias ++ name), bType, num) : importedTypenames) toExportTypenames
      where
        checkTypeNameExists :: String -> TypenameList -> ()
        checkTypeNameExists name typenameList = case typenameList of
          [] -> ()
          (otherName, _, _) : tl ->
            if otherName == name
              then error (currentFile ++ ": Found same type synonym name \"" ++ name ++ "\" imported or declared when importing " ++ toImportFile)
              else checkTypeNameExists name tl

addAlias :: FilePath -> FilePath -> String -> [String] -> TypenameList -> [String]
addAlias currentFile toImportFile alias l typenameList = case l of
  [] -> checkAliasInTypenames currentFile toImportFile alias typenameList
  s : ss ->
    if s == alias
      then error (currentFile ++ ": Found used alias \"" ++ alias ++ "\" when importing " ++ toImportFile)
      else s : addAlias currentFile toImportFile alias ss typenameList

checkAliasInTypenames :: FilePath -> FilePath -> String -> TypenameList -> [String]
checkAliasInTypenames currentFile toImportFile alias l = case l of
  [] -> [alias]
  (str, _, _) : tl ->
    if str == alias
      then error (currentFile ++ ": Found clash between defined type synonyms and alias name \"" ++ alias ++ "\" when importing " ++ toImportFile)
      else checkAliasInTypenames currentFile toImportFile alias tl