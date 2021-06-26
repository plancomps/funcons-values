{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Sets where

import Funcons.Operations.Booleans
import Funcons.Operations.Internal hiding (set_)

import Data.List (permutations)
import qualified Data.Set as S

library :: (HasValues t, Ord t) => Library t
library = libFromList [
    ("set-empty", NullaryExpr set_empty)
  , ("sets", UnaryExpr Funcons.Operations.Sets.sets) 
  , ("is-in-set", BinaryExpr is_in_set) 
  , ("set", NaryExpr set_)
  , ("set-elements", UnaryExpr set_elements)
  , ("is-subset", BinaryExpr is_subset)
  , ("set-insert", BinaryExpr set_insert)
  , ("set-unite", NaryExpr set_unite_)
  , ("set-intersect", NaryExpr set_intersect_)
  , ("set-difference", NaryExpr set_difference_)
  , ("set-size", UnaryExpr set_size)
  , ("some-element", UnaryExpr some_element)
  , ("element-not-in", BinaryExpr element_not_in)
--  , ("is-set-empty", UnaryExpr is_set_empty)
  ]

sets_ :: HasValues t => [OpExpr t] -> OpExpr t
sets_ = unaryOp Funcons.Operations.Sets.sets
sets :: HasValues t => OpExpr t -> OpExpr t
sets = vUnaryOp "sets" op
  where op (ComputationType (Type t)) = 
          Normal $ injectT $ Funcons.Operations.Internal.sets t
        op _ = SortErr "sets not applied to a type" 

set_empty_ :: HasValues t => [OpExpr t] -> OpExpr t
set_empty_ = nullaryOp set_empty
set_empty :: HasValues t => OpExpr t
set_empty = NullaryOp "set-empty" (Normal $ inject (Set S.empty))

is_in_set_ :: (HasValues t, Ord t) => [OpExpr t] -> OpExpr t
is_in_set_ = binaryOp is_in_set
is_in_set :: (HasValues t, Ord t) => OpExpr t -> OpExpr t -> OpExpr t 
is_in_set = BinaryOp "is-in-set" op 
  where op e' s' 
          | Just s <- project s', Just e <- project e' = case s of 
            Set set -> Normal $ inject $ tobool (e `S.member` set)
            _       -> SortErr "is-in-set(V,S) not applied to a value and a set"
          | otherwise = ProjErr "is-in-set"

set_elements_ :: HasValues t => [OpExpr t] -> OpExpr t
set_elements_ = unaryOp set_elements
set_elements :: HasValues t => OpExpr t -> OpExpr t
set_elements = vUnaryOp "set-elements" op
 where op (Set s) = Nondeterministic $ map (Normal . inject . multi 
                                             . map inject) $ 
                     permutations $ S.toList s
       op _ = SortErr "set-elements not applied to a set"

set_size_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
set_size_ = unaryOp set_size
set_size :: (Ord t, HasValues t) => OpExpr t -> OpExpr t
set_size = vUnaryOp "set-size" op
 where op (Set s) = Normal $ inject $ Nat (toInteger $ S.size s) 
       op _ = SortErr "set-size not applied to a set"

set_intersect_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
set_intersect_ = vNaryOp "set-intersect" op
  where op [] = SortErr "set-intersect applied to an empty sequence of sets"
        op vs | all isSet_ vs = Normal $ inject $ 
                                  Set (foldr1 S.intersection (map toSet vs))
              | otherwise = SortErr "set-intersect not applied to sets"
          where isSet_ (Set _) = True
                isSet_ _       = False
                toSet (Set s)  = s
                toSet _        = error "set-intersect toSet"

set_difference_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
set_difference_ = binaryOp set_difference
set_difference :: (Ord t, HasValues t) => OpExpr t -> OpExpr t -> OpExpr t
set_difference = vBinaryOp "set-difference" op
  where op (Set s1) (Set s2) = Normal $ inject $ Set (s1 `S.difference` s2)
        op _ _ = SortErr "set-difference not applied to two sets"

some_element_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
some_element_ = unaryOp some_element
some_element :: (HasValues t, Ord t) => OpExpr t -> OpExpr t
some_element = vUnaryOp "some-element" op
  where op (Set s) | not (S.null s) = choice $ map (Normal . inject) $ S.toList s
                   | otherwise      = Normal $ inject null__
        op _ = SortErr "some-element not applied to a set"

is_subset_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
is_subset_ = binaryOp is_subset
is_subset :: (Ord t, HasValues t) => OpExpr t -> OpExpr t -> OpExpr t
is_subset = vBinaryOp "is-subset" op
  where op (Set s1) (Set s2) = Normal $ inject $ tobool (s1 `S.isSubsetOf` s2)
        op _ _ = SortErr "is-subset not applied to two sets"

set_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
set_ = vNaryOp "set" op
  where op vs = Normal $ inject $ Set (S.fromList vs)

set_unite_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
set_unite_ = vNaryOp "set-unite" op
  where op vs | all isSet_ vs = Normal $ inject $ Set $ S.unions $ map unSet vs
              | otherwise = SortErr "set-unite not applied to sets"
          where isSet_ (Set s) = True
                isSet_ _       = False
                unSet (Set s) = s
                unSet _       = error "set-unite not applied to sets only"

set_insert_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
set_insert_ = binaryOp set_insert
set_insert :: (HasValues t, Ord t) => OpExpr t -> OpExpr t -> OpExpr t
set_insert = vBinaryOp "set-insert" op
  where op e (Set s) = Normal $ inject $ Set (e `S.insert` s)
        op _ _ = SortErr "second argument of set-insert is not a set"

element_not_in_ :: (HasValues t, Ord t) => [OpExpr t] -> OpExpr t 
element_not_in_ = binaryOp element_not_in
element_not_in :: (Ord t, HasValues t) => OpExpr t -> OpExpr t -> OpExpr t
element_not_in = vBinaryOp "element-not-in" op
  where op (ComputationType (Type ty)) (Set set) = case ty of 
          Atoms -> Normal $ inject $ head atoms
          _     -> error "missing case for `element-not-in`"
          where atoms    = dropWhile (flip S.member set) $ 
                              map (Atom . ("@" ++) . show) [1..]
        op _ _ = SortErr "element-not-in not applied to a type and a set"
