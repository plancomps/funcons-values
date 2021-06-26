{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Tuples where

import Funcons.Operations.Internal
import Funcons.Operations.Booleans

library :: HasValues t => Library t
library = libFromList [
    ("tuples", NaryExpr tuples_)
  , ("tuple", NaryExpr tuple_)
  , ("tuple-index", BinaryExpr tuple_index)
  , ("empty-tuple", NullaryExpr  empty_tuple)
  , ("tuple-prepend", BinaryExpr tuple_prepend)
  , ("is-empty", UnaryExpr tuple_is_empty)
  , ("tuple-head", UnaryExpr tupleHeadOp)
  , ("tuple-tail", UnaryExpr tupleTailOp)
  ]

tuples_ :: HasValues t => [OpExpr t] -> OpExpr t
tuples_ = NaryOp "tuples" (Normal . injectT . ADT "tuples")

tuple_is_empty_ :: HasValues t => [OpExpr t] -> OpExpr t
tuple_is_empty_ = unaryOp tuple_is_empty
tuple_is_empty :: HasValues t => OpExpr t -> OpExpr t 
tuple_is_empty = vUnaryOp "is-empty" op
  where op (ADTVal "tuple" vs) = Normal $ inject $ tobool (null vs)
        op _ = SortErr "is-empty not applied to a tuple" 

empty_tuple_, tuple_prepend_ :: HasValues t => [OpExpr t] -> OpExpr t
empty_tuple_ = nullaryOp empty_tuple
tuple_prepend_ = binaryOp tuple_prepend

empty_tuple :: HasValues t => OpExpr t
empty_tuple = vNullaryOp "empty-tuple" (Normal $ inject (tuple []))

tuple_prepend :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
tuple_prepend = vBinaryOp "tuple-prepend" op
  where op v (ADTVal "tuple" vs) = Normal $ inject (ADTVal "tuple" (inject v : vs))
        op _ _ = SortErr "tuple-prepend not applied to a value and a tuple"

tuple_ :: HasValues t => [OpExpr t] -> OpExpr t
tuple_ = vNaryOp "tuple" op
  where op ys = Normal $ inject (tuple ys)

tuple_index_ :: HasValues t => [OpExpr t] -> OpExpr t
tuple_index_ = binaryOp tuple_index
tuple_index :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
tuple_index = vBinaryOp "tuple-index" op
  where op (ADTVal "tuple" ts) v 
          | Nat n' <- upcastNaturals v, let n :: Int; n = fromInteger n'
            = case () of 
               () | n >= 1 && n <= length ts -> Normal $ ts !! (n - 1)
               _ -> SortErr "tuple-index not in range"
          | otherwise = SortErr ("tuple-index not applied to a natural number: " ++ ppValues (const "_") v)
        op _ _ = SortErr "tuple-index not applied to a tuple"

tuple_head_, tuple_tail_ :: HasValues t => [OpExpr t] -> OpExpr t
tuple_head_ = unaryOp tupleHeadOp
tuple_tail_ = unaryOp tupleTailOp
tupleHeadOp,tupleTailOp :: HasValues t => OpExpr t -> OpExpr t
tupleHeadOp = vUnaryOp "head" op
  where op (ADTVal "tuple" [])      = DomErr "tuple-head of empty tuple"
        op (ADTVal "tuple" (x:xs))  = Normal x
        op _                        = SortErr "tuple-head not applied to a tuple"
tupleTailOp = vUnaryOp "tail" op
  where op (ADTVal "tuple" [])      = DomErr "tupletail of empty tuple"
        op (ADTVal "tuple" (x:xs))  = Normal $ inject (ADTVal "tuple" xs)
        op _                        = SortErr "tuple-tail not applied to a tuple"
