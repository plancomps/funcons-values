{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.NonGroundValues where

import Prelude hiding (non_grounded)
import Funcons.Operations.Internal

library :: (HasValues t, Eq t) => Library t
library = libFromList [
    ("non-grounded", UnaryExpr non_grounded)
  , ("non-grounded-values", NullaryExpr non_grounded_values)
  ]

non_grounded_ :: HasValues t => [OpExpr t] -> OpExpr t
non_grounded_ = unaryOp non_grounded 
non_grounded :: HasValues t => OpExpr t -> OpExpr t
non_grounded = vUnaryOp "non-grounded" (Normal . inject . ADTVal "non-grounded" . (:[]) . inject) 

non_grounded_values_ :: HasValues t => [OpExpr t] -> OpExpr t
non_grounded_values_ = nullaryOp non_grounded_values 
non_grounded_values :: HasValues t => OpExpr t
non_grounded_values = vNullaryOp "non-grounded-values" 
  (Normal $ injectT $ ADT "non-grounded-values" [])

{-
-- This function differs from Funcons.Operations.Values.isGround
-- and assumes that non_grounded<> is the only non-ground value constructor
isGround :: Values t -> Bool
isGround (ADTVal "non-grounded" _) = False
isGround v = True
-}
