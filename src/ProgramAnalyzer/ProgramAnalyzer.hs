module ProgramAnalyzer.ProgramAnalyzer where

import AscriptionSimplifier.AscriptionSimplifier
import Datatype
import ExpTypeConverters.ABExpConverter
import Parser.MainParser
import ProgramAnalyzer.LetStatementAnalyzerUtils
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
    return (mainParser clean_file_content)

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
    let (cExp, bType) = mainTypeChecker (src_path ++ currentFile) functionName (TokenlessContext (toContextElemList (importedFunctions ++ toExportFunctions))) [] bExp
    putStrLn (show (bExp))
    putStrLn (show (cExp))
    error "TODO: let Statement analyzer in works"
  where
    toContextElemList :: TypeCheckedProgram -> ContextElemList
    toContextElemList l = case l of
      [] -> []
      (name, cExp, bType, propl) : tl -> (name, bType, Just (cExp, propl)) : toContextElemList tl

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
  error "typeStatementAnalyzer not implemented yet"

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
importStatementAnalyzer toImportFile src_path currentFile compiledFilesData toCompileFiles importedFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl =
  error "importStatementAnalyzer not implemented yet"