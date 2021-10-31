module TypeFunctions.TypeCompare where

import Datatype

generalBTypeCompare :: BType -> BType -> Bool
generalBTypeCompare =
  --TODO: improve this for isorecursive types!
  alphaBTypeCompare

alphaBTypeCompare :: BType -> BType -> Bool
alphaBTypeCompare b1 b2 = b1 == b2
