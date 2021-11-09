--Module to be modified

module Interpreter.StoreFunctions where

import Datatype

elemStore :: StoreElemList -> Integer -> CExp
elemStore s n = case s of
  [] -> error "Should not happen! cannot find the cExp in store!"
  (ind, exp) : x1 -> if ind == n then exp else elemStore x1 n

addStoreElem :: Store -> CExp -> (Store, CExp)
addStoreElem s exp = case s of
  NullStore -> error "Should not happen! addStoreElem called on a NullStore!"
  TicklessStore x0 ->
    do
      let (x0', l) = addStoreElemHelper x0 exp
      (TicklessStore x0', CExpLocation l)
  TickStore x0 x1 ->
    do
      let (x1', l) = addStoreElemHelper x1 exp
      (TickStore x0 x1', CExpLocation l)
  where
    addStoreElemHelper :: StoreElemList -> CExp -> (StoreElemList, Integer)
    addStoreElemHelper l exp = case l of
      [] -> ([(0, exp)], 0)
      (n, _) : x1 -> ((n + 1, exp) : l, n + 1)

modifyStoreElem :: StoreElemList -> Integer -> CExp -> StoreElemList
modifyStoreElem sList key cExp = case sList of
  [] -> []
  (key', load) : x1 -> if key' == key then (key, cExp) : x1 else (key', load) : modifyStoreElem x1 key cExp

transformInputStore :: Store -> CExp -> CExp -> (Store, CExp)
-- take a tickless store, input and l, produce a tickstore and l'
transformInputStore s input l =
  case s of
    TicklessStore lis -> do
      let (s', l') = addStoreElem (TickStore lis []) CExpUnit
      let TickStore a b = s'
      let CExpLocation n = l
      let a' = modifyStoreElem a n (CExpInto (CExpProduct input l'))
      let newStore = TickStore a' b
      (newStore, l')
    _ -> error "store passed to transform input store is not tickless"

{-
removeStoreElem :: StoreElemList -> Integer -> StoreElemList
removeStoreElem s n =
  case s of
    [] -> error "Should not happen! Cannot find a particular store element to remove"
    (ind, exp) : tl -> if ind == n then tl else (ind, exp) : (removeStoreElem tl n)
-}