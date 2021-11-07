module MainFunctions.GetMain where

import Datatype

getMain :: CompiledFilesData -> (CExp, BType)
getMain compiledFilesData =
  do
    let (_, entryFileData, _) : _ = compiledFilesData
    getMainHelper entryFileData
  where
    getMainHelper :: TypeCheckedProgram -> (CExp, BType)
    getMainHelper l = case l of
      [] -> error "Can't find main program in entry file"
      (name, cExp, bType, tps) : x1 ->
        if "main" /= name
          then getMainHelper x1
          else if length tps /= 0 then error "main program can't be polymorphic!" else (cExp, bType)