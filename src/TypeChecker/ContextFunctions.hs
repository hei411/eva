module TypeChecker.ContextFunctions where

import Datatype

addContextElem :: Context -> ContextElem -> Context
addContextElem c cell = case c of
  TokenlessContext x0 -> TokenlessContext (cell : x0)
  StableContext x0 x1 -> StableContext x0 (cell : x1)
  AngleContext x0 x1 x2 -> AngleContext x0 x1 (cell : x2)
  AtContext x0 x1 x2 -> AtContext x0 x1 (cell : x2)

elemContext :: ContextElemList -> String -> Maybe (BType, Integer)
elemContext l s = case l of
  [] -> Nothing
  (var, t, n) : x1 -> if var == s then Just (t, n) else elemContext x1 s
