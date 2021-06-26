{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Optionals where

import Funcons.Operations.Internal

library :: (HasValues t, Eq t) => Library t
library = libFromList [
    ("optionals", UnaryExpr optionals)
  , ("none", NullaryExpr none)
  , ("some", UnaryExpr some)
  ]

toOpt :: HasValues t => Maybe t -> Values t 
toOpt (Just t)  = ADTVal "some" [t]
toOpt Nothing   = none__ 

none__ = ADTVal "none" []

optionals_ :: HasValues t => [OpExpr t] -> OpExpr t
optionals_ = unaryOp optionals
optionals :: HasValues t => OpExpr t -> OpExpr t
optionals = vUnaryOp "optionals" op
  where op (ComputationType t) = (Normal $ injectT $ ADT "optionals" [injectCT t])
        op _        = SortErr "optionals not applied to a type"

some__ :: HasValues t => t -> Values t 
some__ = toOpt . Just
some_ :: HasValues t => [OpExpr t] -> OpExpr t
some_ = unaryOp some
some :: HasValues t => OpExpr t -> OpExpr t
some = vUnaryOp "some" (Normal . inject . some__ . inject)

none_ :: HasValues t => [OpExpr t] -> OpExpr t
none_ = nullaryOp none
none :: HasValues t => OpExpr t
none = vNullaryOp "none" (Normal $ inject $ none__)


