module ProgramAnalyzer.ProgramAnalyzer where

import Datatype
import TypeChecker.MainTypeChecker
import TypeChecker.ValidChecker

mainProgramAnalyzer :: Program -> IO TypeCheckedProgram
mainProgramAnalyzer l =
  --need to insert current file name into toCompileFiles
  mainProgramAnalyzerHelper [] [] [] [] l

mainProgramAnalyzerHelper :: [FilePath] -> [FilePath] -> TypeCheckedProgram -> TypeCheckedProgram -> Program -> IO TypeCheckedProgram
mainProgramAnalyzerHelper compiledFiles toCompileFiles importedFunctions toExportFunctions l =
  case l of
    [] -> return toExportFunctions
    hd : tl -> case hd of
      LetStatement var exp ->
        do
          let validTypes = isValidExp exp
          case validTypes of
            Right at -> fail (var ++ " has invalid type ascription: " ++ show (at) ++ "!")
            Left x0 ->
              do
                let t = mainTypeChecker (importedFunctions ++ toExportFunctions) exp
                case t of
                  Nothing -> fail (var ++ " cannot be type-checked correctly.")
                  Just checkedType ->
                    mainProgramAnalyzerHelper compiledFiles toCompileFiles importedFunctions ((var, exp, checkedType) : toExportFunctions) tl
      -- For future alternative statements
      _ -> mainProgramAnalyzer tl