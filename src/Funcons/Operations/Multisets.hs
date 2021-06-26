
module Funcons.Operations.Multisets where

import Funcons.Operations.Booleans
import Funcons.Operations.Internal

import qualified Data.MultiSet as MS

library :: (HasValues t, Ord t) => Library t
library = libFromList [
    ("multisets", UnaryExpr Funcons.Operations.Multisets.multisets)  
  , ("multiset", NaryExpr multiset_)
  , ("multiset-elements", UnaryExpr multiset_elements)
  , ("multiset-occurrences", BinaryExpr multiset_occurrences)
  , ("multiset-insert", BinaryExpr multiset_insert)
  , ("multiset-delete", TernaryExpr multiset_delete)
  , ("is-submultiset", BinaryExpr is_submultiset)
  ]

multisets_ :: HasValues t => [OpExpr t] -> OpExpr t
multisets_ = unaryOp Funcons.Operations.Multisets.multisets
multisets :: HasValues t => OpExpr t -> OpExpr t
multisets = vUnaryOp "multisets" op   
  where op (ComputationType (Type t)) = 
          Normal $ injectT $ Funcons.Operations.Internal.multisets t
        op _ = SortErr "multisets not applied to a type" 

multiset_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
multiset_ = vNaryOp "multiset" op
  where op vs = Normal $ inject $ Multiset (MS.fromList vs)

multiset_elements_ :: HasValues t => [OpExpr t] -> OpExpr t
multiset_elements_ = unaryOp multiset_elements
multiset_elements :: HasValues t => OpExpr t -> OpExpr t
multiset_elements = vUnaryOp "multiset-elements" op
 where op (Multiset s) = Normal $ inject $ multi $ map inject $ MS.toList s
       op _ = SortErr "multiset-elements not applied to a multiset"

multiset_occurrences_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
multiset_occurrences_ = binaryOp multiset_occurrences
multiset_occurrences :: (Ord t, HasValues t) => OpExpr t -> OpExpr t -> OpExpr t
multiset_occurrences = vBinaryOp "multiset-occurrences" op
  where op v (Multiset ms) = Normal $ inject $ Int count 
          where count = toInteger $ MS.occur v ms 
        op _ _ = SortErr "multiset-occurrences not applied to a value and a multiset"

multiset_insert_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
multiset_insert_ = binaryOp multiset_insert
multiset_insert :: (HasValues t, Ord t) => OpExpr t -> OpExpr t -> OpExpr t
multiset_insert = vBinaryOp "multiset-insert" op
  where op e (Multiset s) = Normal $ inject $ Multiset (e `MS.insert` s)
        op _ _ = SortErr "second argument of multiset-insert is not a multiset"

multiset_delete_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
multiset_delete_ = ternaryOp multiset_delete
multiset_delete :: (Ord t, HasValues t) => OpExpr t -> OpExpr t -> OpExpr t -> OpExpr t
multiset_delete = vTernaryOp "multiset-delete" op
  where op (Multiset s) gv x | Nat n <- upcastNaturals x
          = Normal $ inject $ Multiset (MS.deleteMany gv (fromInteger n) s)
        op _ _ _ = SortErr "multiset-delete not applied to a multiset, a potential element, and a natural number"

is_submultiset_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
is_submultiset_ = binaryOp is_submultiset
is_submultiset :: (Ord t, HasValues t) => OpExpr t -> OpExpr t -> OpExpr t
is_submultiset = vBinaryOp "is-submultiset" op
  where op (Multiset s1) (Multiset s2) = Normal $ inject $ tobool (s1 `MS.isSubsetOf` s2)
        op _ _ = SortErr "is-submultiset not applied to two multisets"


