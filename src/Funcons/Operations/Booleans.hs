{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Booleans where

import Funcons.Operations.Internal

library :: (HasValues t, Eq t) => Library t
library = libFromList [
    ("is-equal", BinaryExpr is_equal)
--  , ("booleans", NullaryExpr booleans_)
--  , ("and", BinaryExpr stepAnd)
  ]

tobool :: Bool -> Values t 
tobool True   = ADTVal "true" []
tobool False  = ADTVal "false" [] 

frombool :: (Values t) -> Maybe Bool
frombool (ADTVal "true" []) = Just True
frombool (ADTVal "false" []) = Just False
frombool _ = Nothing


{-
and_ :: HasValues t => [OpExpr t] -> OpExpr t 
and_ = binaryOp stepAnd
stepAnd :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
stepAnd = vBinaryOp "and" op
  where op x y = case (frombool x, frombool y) of 
          (Just b1, Just b2) -> Normal $ inject $ tobool (b1 && b2)
          _ -> SortErr "and not applied to two booleans"
-}
booleans_ :: HasValues t => OpExpr t
booleans_ = vNullaryOp "booleans" (Normal $ injectT $ ADT "booleans" [])

true_ :: HasValues t => Values t 
true_ = tobool True

false_ :: HasValues t => Values t 
false_ = tobool False 


is_equal_ :: (HasValues t, Eq t) => [OpExpr t] -> OpExpr t
is_equal_ = binaryOp is_equal
is_equal :: (HasValues t, Eq t) => OpExpr t -> OpExpr t -> OpExpr t
is_equal = BinaryOp "is-equal" op 
  where op :: (Eq t, HasValues t) => t -> t -> Result t
        op x y = Normal $ inject $ tobool (x == y)


