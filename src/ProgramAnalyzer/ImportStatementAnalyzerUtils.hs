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

addFunctions :: [Char] -> FilePath -> TypeCheckedProgram -> TypeCheckedProgram -> TypeCheckedProgram -> TypeCheckedProgram
addFunctions currentFile toImportFile toImportFunctions importedFunctions toExportFunctions =
  case toImportFunctions of
    [] -> importedFunctions
    (name, cExp, bType, typeProperties) : tl ->
      do
        let _ = checkFunctionNameExists name (importedFunctions ++ toExportFunctions)
        addFunctions currentFile toImportFile tl ((name, cExp, bType, typeProperties) : importedFunctions) toExportFunctions
      where
        checkFunctionNameExists :: String -> TypeCheckedProgram -> ()
        checkFunctionNameExists name functionList = case functionList of
          [] -> ()
          (otherName, _, _, _) : tl ->
            if otherName == name
              then error (currentFile ++ ": Found same function name \"" ++ name ++ "\" imported or declared when importing " ++ toImportFile)
              else checkFunctionNameExists name tl

addTypenames :: [Char] -> FilePath -> TypenameList -> TypenameList -> TypenameList -> TypenameList
addTypenames currentFile toImportFile toImportTypenames importedTypenames toExportTypenames =
  case toImportTypenames of
    [] -> importedTypenames
    (name, bType, num) : tl ->
      do
        let _ = checkTypeNameExists name (importedTypenames ++ toExportTypenames)
        addTypenames currentFile toImportFile tl ((name, bType, num) : importedTypenames) toExportTypenames
      where
        checkTypeNameExists :: String -> TypenameList -> ()
        checkTypeNameExists name typenameList = case typenameList of
          [] -> ()
          (otherName, _, _) : tl ->
            if otherName == name
              then error (currentFile ++ ": Found same type synonym name \"" ++ name ++ "\" imported or declared when importing " ++ toImportFile)
              else checkTypeNameExists name tl
