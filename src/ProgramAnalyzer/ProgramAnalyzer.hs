module ProgramAnalyzer.ProgramAnalyzer where

import Datatype
import ExpTypeConverters.ABExpConverter
import ExpTypeConverters.TypeSynonymConverter
import Parser.MainParser
import ProgramAnalyzer.DefStatementAnalyzerUtils
import ProgramAnalyzer.ImportStatementAnalyzerUtils
import ProgramAnalyzer.TypeStatementAnalyzerUtils
import StringFunctions.CommentHandler
import qualified Text.Parsec as Text.Parsec.Error
import TypeChecker.MainTypeChecker

--TODO: need to fix sourcepath outputs!
mainProgramAnalyzer :: String -> String -> IO CompiledFilesData
mainProgramAnalyzer src_path file_name =
  --need to insert current file name into toCompileFiles
  mainProgramAnalyzerHelper src_path file_name [] [file_name]

mainProgramAnalyzerHelper ::
  FilePath ->
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  IO CompiledFilesData
mainProgramAnalyzerHelper src_path currentFile compiledFilesData toCompileFiles =
  do
    --putStrLn ("Parsing " ++ src_path ++ currentFile)
    parse_tree <- readParse currentFile
    case parse_tree of
      -- Error in parsing
      Left parseError -> error (show (parseError))
      Right program -> do
        -- Parsing succeeded
        -- putStrLn (show (program))
        --putStrLn (currentFile ++ " is parsed correctly")
        singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles [] [] [] [] [] [] program

readParse :: FilePath -> IO (Either Text.Parsec.Error.ParseError Program)
readParse fileName =
  do
    -- Read from file
    file_content <- readFile (fileName)
    -- Remove comments
    let clean_file_content = commentRemover file_content
    -- Create parse tree
    return (mainParser fileName clean_file_content)

singleFileAnalyzer ::
  FilePath ->
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  [FilePath] ->
  [String] ->
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO CompiledFilesData
singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles importedFiles usedAlias importedFunctions toExportFunctions importedTypenames toExportTypenames program = case program of
  [] ->
    do
      --putStrLn (src_path ++ currentFile ++ " completely typechecked.")
      return ((currentFile, toExportFunctions, toExportTypenames) : compiledFilesData)
  hd : tl -> case hd of
    DefStatement str polyParams aExp -> defStatementAnalyzer str polyParams aExp src_path currentFile compiledFilesData toCompileFiles importedFiles usedAlias importedFunctions toExportFunctions importedTypenames toExportTypenames tl
    TypeStatement str typeParams aType -> typeStatementAnalyzer str typeParams aType src_path currentFile compiledFilesData toCompileFiles importedFiles usedAlias importedFunctions toExportFunctions importedTypenames toExportTypenames tl
    ImportStatement fileName potentialAlias -> importStatementAnalyzer fileName src_path currentFile potentialAlias compiledFilesData toCompileFiles importedFiles usedAlias importedFunctions toExportFunctions importedTypenames toExportTypenames tl

{-case l of
          -- return compiled files and their functions,  type names and functions
          [] -> return (compiledFilesData)
          hd : tl -> case hd of
            LetStatement var parameterList aexp ->
              do
                --Step 1: Convert AExp to BExp, i.e. type ascriptions are converted to BTypes (all db indices, even forall)
                --let bExp = abExpConverter (importedTypenames ++ toExportTypenames) aexp
                --Step 2: Type check BExp while producing CExp that is substituted with correct previously declared functions
                -- let (cExp, bType) = mainTypeChecker (importedFunctions ++ toExportFunctions) (TokenlessContext []) bExpSimplified
                --Step 3: Interpret CExp
                --Ignore below
                -- Resolve all type ascriptions
                --putStrLn (show (cExp))
                return (compiledFilesData)
            {-
              let validTypes = isValidExp (importedTypenames ++ toExportTypenames) exp
              case validTypes of
                Right at -> fail (var ++ " has invalid type ascription: " ++ show (at) ++ "!")
                Left x0 ->
                  do
                    -- Type check exp
                    let t = mainTypeChecker (importedFunctions ++ toExportFunctions) exp
                    case t of
                      Nothing -> fail (var ++ " cannot be type-checked.\n" ++ show (exp))
                      Just checkedType ->
                        -- TODO: do substitution for predefined functions
                        mainProgramAnalyzerHelper currentFile compiledFiles toCompileFiles importedFunctions ((var, exp, checkedType) : toExportFunctions) importedTypenames toExportTypenames tl
            -}
            -- For future alternative statements
            _ -> mainProgramAnalyzerHelper currentFile compiledFilesData toCompileFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl
-}
defStatementAnalyzer ::
  String ->
  [(TypeProperty, String)] ->
  AExp ->
  FilePath ->
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  [FilePath] ->
  [String] ->
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO CompiledFilesData
defStatementAnalyzer functionName polyParams aExp src_path currentFile compiledFilesData toCompileFiles importedFiles usedAlias importedFunctions toExportFunctions importedTypenames toExportTypenames tl =
  do
    checkFunctionNameExists (currentFile) (importedFunctions ++ toExportFunctions) functionName
    let bExp = abExpConverter (currentFile) functionName polyParams (importedTypenames ++ toExportTypenames) aExp
    let (cExp, bType) = mainTypeChecker (currentFile) functionName (importedFunctions ++ toExportFunctions) (TokenlessContext []) [] bExp
    let typeProperties = map fst polyParams
    let newToExportFunctions = (functionName, cExp, bType, typeProperties) : toExportFunctions
    singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles importedFiles usedAlias importedFunctions newToExportFunctions importedTypenames toExportTypenames tl

typeStatementAnalyzer ::
  String ->
  [String] ->
  AType ->
  FilePath ->
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  [FilePath] ->
  [String] ->
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO CompiledFilesData
typeStatementAnalyzer typeSynonymName typeVariables aType src_path currentFile compiledFilesData toCompileFiles importedFiles usedAlias importedFunctions toExportFunctions importedTypenames toExportTypenames tl =
  do
    checkTypeSynonymNameExists (currentFile) (importedTypenames ++ toExportTypenames) typeSynonymName
    let bType = typeSynonymConverter (currentFile) typeSynonymName typeVariables (importedTypenames ++ toExportTypenames) [] aType
    let newToExportTypenames = (typeSynonymName, bType, toInteger (length typeVariables)) : toExportTypenames
    singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles importedFiles usedAlias importedFunctions toExportFunctions importedTypenames newToExportTypenames tl

importStatementAnalyzer ::
  FilePath ->
  FilePath ->
  FilePath ->
  Maybe (String) ->
  CompiledFilesData ->
  [FilePath] ->
  [FilePath] ->
  [String] ->
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO CompiledFilesData
importStatementAnalyzer
  toImportFile
  src_path
  currentFile
  potentialAlias
  compiledFilesData
  toCompileFiles
  importedFiles
  usedAlias
  importedFunctions
  toExportFunctions
  importedTypenames
  toExportTypenames
  tl =
    do
      putStrLn ("Attempting to import " ++ src_path ++ toImportFile ++ " from " ++ currentFile)
      checkCircularDependency (src_path ++ toImportFile) (currentFile) toCompileFiles
      if elem (src_path ++ toImportFile) importedFiles
        then do
          putStrLn ((currentFile) ++ ": " ++ (src_path ++ toImportFile) ++ " already imported. Import statement is ignored.")
          singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles importedFiles usedAlias importedFunctions toExportFunctions importedTypenames toExportTypenames tl
        else do
          let fileData = findFileData (src_path ++ toImportFile) compiledFilesData
          let newImportedFiles = (src_path ++ toImportFile) : importedFiles
          let (newUsedAlias, alias) = case potentialAlias of
                Nothing -> (usedAlias, "")
                Just s -> (addAlias (currentFile) (src_path ++ toImportFile) s usedAlias, s ++ ['.'])
          case fileData of
            Nothing ->
              do
                newCompiledFilesData <- mainProgramAnalyzerHelper src_path (src_path ++ toImportFile) compiledFilesData ((src_path ++ toImportFile) : toCompileFiles)

                let newFileData = findFileData (src_path ++ toImportFile) newCompiledFilesData
                case newFileData of
                  Nothing -> error (currentFile ++ ": Should not happen! Cannot find " ++ src_path ++ toImportFile ++ " data after importing it!")
                  Just (functions, typeSynonyms) ->
                    addImportedFileData functions typeSynonyms toImportFile alias src_path currentFile newCompiledFilesData toCompileFiles newImportedFiles newUsedAlias importedFunctions toExportFunctions importedTypenames toExportTypenames tl
            --already compiled
            Just (functions, typeSynonyms) ->
              addImportedFileData functions typeSynonyms toImportFile alias src_path currentFile compiledFilesData toCompileFiles newImportedFiles newUsedAlias importedFunctions toExportFunctions importedTypenames toExportTypenames tl

addImportedFileData ::
  TypeCheckedProgram ->
  TypenameList ->
  FilePath ->
  String ->
  FilePath ->
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  [FilePath] ->
  [String] ->
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO CompiledFilesData
addImportedFileData
  toAddFunctions
  toAddTypeSynonyms
  toImportFile
  alias
  src_path
  currentFile
  compiledFilesData
  toCompileFiles
  importedFiles
  usedAlias
  importedFunctions
  toExportFunctions
  importedTypenames
  toExportTypenames
  tl =
    do
      let newImportedFunctions = addFunctions (currentFile) (src_path ++ toImportFile) alias toAddFunctions importedFunctions toExportFunctions
      let newImportedTypenames = addTypenames (currentFile) (src_path ++ toImportFile) alias toAddTypeSynonyms importedTypenames toExportTypenames
      singleFileAnalyzer
        src_path
        currentFile
        compiledFilesData
        toCompileFiles
        usedAlias
        importedFiles
        newImportedFunctions
        toExportFunctions
        newImportedTypenames
        toExportTypenames
        tl
