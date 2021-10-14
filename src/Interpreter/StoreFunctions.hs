module Interpreter.StoreFunctions where

import Datatype

elemStore :: StoreElemList -> Integer -> Maybe AExp
elemStore s n = case s of
  [] -> Nothing
  (ind, exp) : x1 -> if ind == n then return exp else elemStore x1 n

addStoreElem :: Store -> AExp -> Maybe (Store, AExp)
addStoreElem s exp = case s of
  NullStore -> Nothing
  TicklessStore x0 ->
    do
      let (x0', l) = addStoreElemHelper x0 exp
      return (TicklessStore x0', AExpLocation l)
  TickStore x0 x1 ->
    do
      let (x1', l) = addStoreElemHelper x1 exp
      return (TickStore x0 x1', AExpLocation l)
  where
    addStoreElemHelper :: StoreElemList -> AExp -> (StoreElemList, Integer)
    addStoreElemHelper l exp = case l of
      [] -> ([(0, exp)], 0)
      (n, _) : x1 -> ((n + 1, exp) : l, n + 1)
