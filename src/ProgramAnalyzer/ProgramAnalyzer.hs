module ProgramAnalyzer.ProgramAnalyzer where

import Datatype
import ExpTypeConverters.ABExpConverter

mainProgramAnalyzer :: String -> Program -> IO CompiledFilesData
mainProgramAnalyzer file_name l =
  --need to insert current file name into toCompileFiles
  mainProgramAnalyzerHelper file_name [] [file_name] [] [] [] [] l

mainProgramAnalyzerHelper ::
  FilePath ->
  CompiledFilesData ->
  [FilePath] ->
  TypeCheckedProgram ->
  TypeCheckedProgram ->
  TypenameList ->
  TypenameList ->
  Program ->
  IO CompiledFilesData
mainProgramAnalyzerHelper currentFile compiledFilesData toCompileFiles importedFunctions toExportFunctions importedTypenames toExportTypenames l =
  case l of
    -- return compiled files and their functions,  type names and functions
    [] -> return (compiledFilesData)
    hd : tl -> case hd of
      LetStatement var aexp ->
        do
          --Step 1: Convert AExp to BExp, i.e. type ascriptions are converted to BTypes
          let bExp = abExpConverter (importedTypenames ++ toExportTypenames) aexp
          return (compiledFilesData)
      --Step 2: beta reduce BTypes ascriptions
      --Step 3: Type check BExp while producing CExp
      --Step 4: Interpret CExp
      --Ignore below
      -- Resolve all type ascriptions

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
