{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Lists where

import Funcons.Operations.Internal
import Funcons.Operations.Booleans

import Data.Maybe (isJust, fromJust)

library :: HasValues t => Library t
library = libFromList [
    ("lists", UnaryExpr lists)
  , ("list-singleton", UnaryExpr list_singleton)
  , ("list", NaryExpr list_)
  , ("list-append", BinaryExpr list_append)
  , ("list-concat", NaryExpr list_concat)
  , ("nil", NullaryExpr nil)
  , ("cons", BinaryExpr cons)
  , ("is-nil", UnaryExpr is_nil)
  , ("head", UnaryExpr headOp)
  , ("tail", UnaryExpr tailOp)
  ]

lists_ :: HasValues t => [OpExpr t] -> OpExpr t
lists_ = unaryOp lists
lists :: HasValues t => OpExpr t -> OpExpr t
lists = UnaryOp "lists" (Normal . injectT . ADT "lists" . (:[]))

list_singleton_ :: HasValues t => [OpExpr t] -> OpExpr t 
list_singleton_ = unaryOp list_singleton
list_singleton :: HasValues t => OpExpr t -> OpExpr t
list_singleton = vUnaryOp "list-singleton" (Normal . inject . list . (:[]))

nil_ :: HasValues t => [OpExpr t] -> OpExpr t
nil_ = nullaryOp nil
nil :: HasValues t => OpExpr t
nil = NullaryOp "nil" (Normal $ inject $ list [])

is_nil_ :: HasValues t => [OpExpr t] -> OpExpr t
is_nil_ = unaryOp is_nil
is_nil :: HasValues t => OpExpr t -> OpExpr t
is_nil = UnaryOp "is-nil" op
  where op xs | Just lv <- project xs = case lv of 
                  ADTVal "list" [] -> Normal $ inject $ true_ 
                  _                -> Normal $ inject $ false_
              | otherwise = ProjErr "is-nil"

cons_ ::  HasValues t =>[OpExpr t] -> OpExpr t
cons_ = binaryOp cons
cons :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t 
cons = vBinaryOp "cons" op
  where op v lv = case lv of
                    ADTVal "list" xs -> Normal $ inject $ ADTVal "list" (inject v:xs)
                    _ -> SortErr"cons should be given a value and a list"

list_ :: HasValues t => [OpExpr t] -> OpExpr t
list_ = vNaryOp "list" (Normal . inject . list)

list_append_ :: HasValues t => [OpExpr t] -> OpExpr t
list_append_ = binaryOp list_append
list_append :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
list_append = vBinaryOp "list-append" op
   where op (ADTVal "list" l1) (ADTVal "list" l2) = 
            Normal $ inject $ ADTVal "list" (l1 ++ l2)
         op _ _ = SortErr "list-append not applied to two lists"
isList (ADTVal "list" l) = all (isJust) $ map project l 
isList _                 = False
toList (ADTVal "list" l) = map (fromJust . project) l
toList _                 = error "list-append 1"
     
list_concat_ :: HasValues t => [OpExpr t] -> OpExpr t
list_concat_ = list_concat
list_concat :: HasValues t => [OpExpr t] -> OpExpr t
list_concat = vNaryOp "list-concat" op
   where op args | all isList args = Normal $ inject $ list $ concatMap toList args
                 | otherwise       = SortErr "list-concat not applied to lists"

head_, tail_ :: HasValues t => [OpExpr t] -> OpExpr t
head_ = unaryOp headOp
tail_ = unaryOp tailOp
headOp,tailOp :: HasValues t => OpExpr t -> OpExpr t
headOp = vUnaryOp "head" op
  where op (ADTVal "list" [])      = DomErr "head of empty list"
        op (ADTVal "list" (x:xs))  = Normal x
        op _                       = SortErr "head not applied to a list"
tailOp = vUnaryOp "tail" op
  where op (ADTVal "list" [])      = DomErr "tail of empty list"
        op (ADTVal "list" (x:xs))  = Normal $ inject (ADTVal "list" xs)
        op _                       = SortErr "tail not applied to a list"
