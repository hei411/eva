module ProgramAnalyzer.ProgramAnalyzer where

import Datatype
import TypeChecker.MainTypeChecker
import TypeChecker.ValidChecker

mainProgramAnalyzer :: String -> Program -> IO ([FilePath], [FilePath], TypeCheckedProgram, TypenameList)
mainProgramAnalyzer file_name l =
  --need to insert current file name into toCompileFiles
  mainProgramAnalyzerHelper file_name [] [file_name] [] [] [] [] l

mainProgramAnalyzerHelper ::
  FilePath ->
  [FilePath] ->
  [FilePath] ->
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO ([FilePath], [FilePath], TypeCheckedProgram, TypenameList)
mainProgramAnalyzerHelper currentFile compiledFiles toCompileFiles importedFunctions toExportFunctions importedTypenames toExportTypenames l =
  case l of
    [] -> return (currentFile : compiledFiles, removeElem currentFile toCompileFiles, toExportFunctions, toExportTypenames)
    hd : tl -> case hd of
      LetStatement var exp ->
        do
          --Step 1: Convert AExp to BExp, i.e. type ascriptions are converted to BTypes
          --Step 2: beta reduce BTypes ascriptions
          --Step 3: Type check BExp while producing CExp
          --Step 4: Interpret CExp
          --Ignore below
          -- Resolve all type ascriptions
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

      -- For future alternative statements
      _ -> mainProgramAnalyzerHelper currentFile compiledFiles toCompileFiles importedFunctions toExportFunctions importedTypenames toExportTypenames tl

removeElem :: FilePath -> [FilePath] -> [FilePath]
removeElem elem l =
  case l of
    [] -> error "Should not happen. Cannot find filename in toCompileList after compiling it"
    hd : tl -> if hd == elem then tl else hd : (removeElem elem tl)
