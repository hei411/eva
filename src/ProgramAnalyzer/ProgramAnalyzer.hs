module ProgramAnalyzer.ProgramAnalyzer where

import Datatype
import ExpTypeConverters.ABExpConverter
import ExpTypeConverters.TypeSynonymConverter
import Parser.MainParser
import ProgramAnalyzer.ImportStatementAnalyzerUtils
import ProgramAnalyzer.LetStatementAnalyzerUtils
import ProgramAnalyzer.TypeStatementAnalyzerUtils
import StringFunctions.CommentHandler
import qualified Text.Parsec as Text.Parsec.Error
import TypeChecker.MainTypeChecker

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
    parse_tree <- readParse (src_path ++ currentFile)
    case parse_tree of
      -- Error in parsing
      Left parseError -> error (show (parseError))
      Right program -> do
        -- Parsing succeeded
        -- putStrLn (show (program))
        --putStrLn (currentFile ++ " is parsed correctly")
        singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles [] [] [] [] [] program

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
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO CompiledFilesData
singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames program = case program of
  [] ->
    do
      --putStrLn (src_path ++ currentFile ++ " completely typechecked.")
      return ((src_path ++ currentFile, toExportFunctions, toExportTypenames) : compiledFilesData)
  hd : tl -> case hd of
    LetStatement str polyParams aExp -> letStatementAnalyzer str polyParams aExp src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl
    TypeStatement str typeParams aType -> typeStatementAnalyzer str typeParams aType src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl
    ImportStatement fileName -> importStatementAnalyzer fileName src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl

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
letStatementAnalyzer ::
  String ->
  [(TypeProperty, String)] ->
  AExp ->
  FilePath ->
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  [FilePath] ->
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO CompiledFilesData
letStatementAnalyzer functionName polyParams aExp src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl =
  do
    checkFunctionNameExists (src_path ++ currentFile) (importedFunctions ++ toExportFunctions) functionName
    let bExp = abExpConverter (src_path ++ currentFile) functionName polyParams (importedTypenames ++ toExportTypenames) aExp
    let (cExp, bType) = mainTypeChecker (src_path ++ currentFile) functionName (importedFunctions ++ toExportFunctions) (TokenlessContext []) [] bExp
    let typeProperties = map fst polyParams
    let newToExportFunctions = (functionName, cExp, bType, typeProperties) : toExportFunctions
    singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions newToExportFunctions importedTypenames toExportTypenames tl

typeStatementAnalyzer ::
  String ->
  [String] ->
  AType ->
  FilePath ->
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  [FilePath] ->
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO CompiledFilesData
typeStatementAnalyzer typeSynonymName typeVariables aType src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl =
  do
    checkTypeSynonymNameExists (src_path ++ currentFile) (importedTypenames ++ toExportTypenames) typeSynonymName
    let bType = typeSynonymConverter (src_path ++ currentFile) typeSynonymName typeVariables (importedTypenames ++ toExportTypenames) [] aType
    let newToExportTypenames = (typeSynonymName, bType, toInteger (length typeVariables)) : toExportTypenames
    singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions toExportFunctions importedTypenames newToExportTypenames tl

importStatementAnalyzer ::
  FilePath ->
  FilePath ->
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  [FilePath] ->
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
  compiledFilesData
  toCompileFiles
  importedFiles
  importedFunctions
  toExportFunctions
  importedTypenames
  toExportTypenames
  tl =
    do
      putStrLn ("Attempting to import " ++ src_path ++ toImportFile ++ " from " ++ src_path ++ currentFile)
      checkCircularDependency toImportFile (src_path ++ currentFile) toCompileFiles
      if elem toImportFile importedFiles
        then do
          putStrLn ((src_path ++ currentFile) ++ ": " ++ toImportFile ++ " already imported.")
          singleFileAnalyzer src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl
        else do
          let fileData = findFileData toImportFile compiledFilesData
          let newImportedFiles = toImportFile : importedFiles
          case fileData of
            Nothing ->
              do
                newCompiledFilesData <- mainProgramAnalyzerHelper src_path toImportFile compiledFilesData (toImportFile : toCompileFiles)

                let newFileData = findFileData toImportFile newCompiledFilesData
                case newFileData of
                  Nothing -> error (src_path ++ currentFile ++ ": Should not happen! Cannot find " ++ src_path ++ toImportFile ++ " data after importing it!")
                  Just (functions, typeSynonyms) ->
                    addImportedFileData functions typeSynonyms toImportFile src_path currentFile newCompiledFilesData toCompileFiles newImportedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl
            Just (functions, typeSynonyms) ->
              addImportedFileData functions typeSynonyms toImportFile src_path currentFile compiledFilesData toCompileFiles newImportedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl

addImportedFileData ::
  TypeCheckedProgram ->
  TypenameList ->
  FilePath ->
  FilePath ->
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  [FilePath] ->
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
  src_path
  currentFile
  compiledFilesData
  toCompileFiles
  importedFiles
  importedFunctions
  toExportFunctions
  importedTypenames
  toExportTypenames
  tl =
    do
      let newImportedFunctions = addFunctions (src_path ++ currentFile) toImportFile toAddFunctions importedFunctions toExportFunctions
      let newImportedTypenames = addTypenames (src_path ++ currentFile) toImportFile toAddTypeSynonyms importedTypenames toExportTypenames
      singleFileAnalyzer
        src_path
        currentFile
        compiledFilesData
        toCompileFiles
        importedFiles
        newImportedFunctions
        toExportFunctions
        newImportedTypenames
        toExportTypenames
        tl
