module TypeChecker.ContextFunctions where

import Datatype

elemContext :: ContextElemList -> String -> Maybe AType
elemContext l s = case l of
  [] -> Nothing
  (var, t) : x1 -> if var == s then Just t else elemContext x1 s

isTickFree :: Context -> Bool
isTickFree c = case c of
  TokenlessContext x0 -> True
  StableContext x0 x1 -> True
  ArrowContext x0 x1 x2 -> False
  AtContext x0 x1 x2 -> False

addContextElem :: Context -> ContextElem -> Context
addContextElem c cell = case c of
  TokenlessContext x0 -> TokenlessContext (cell : x0)
  StableContext x0 x1 -> StableContext x0 (cell : x1)
  ArrowContext x0 x1 x2 -> ArrowContext x0 x1 (cell : x2)
  AtContext x0 x1 x2 -> AtContext x0 x1 (cell : x2)
