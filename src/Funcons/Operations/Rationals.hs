
module Funcons.Operations.Rationals where

import Funcons.Operations.Booleans
import Funcons.Operations.Internal

library :: HasValues t => Library t
library = libFromList [
    ("rationals", NullaryExpr (vNullaryOp "rationals" (Normal $ injectT $ Rationals)))
  , ("is-less", BinaryExpr is_less)
  , ("is-less-or-equal", BinaryExpr is_less_or_equal)
  , ("is-leq", BinaryExpr is_less_or_equal)
  , ("is-greater", BinaryExpr is_greater)
  , ("is-greater-or-equal", BinaryExpr is_greater_or_equal)
  , ("is-geq", BinaryExpr is_greater_or_equal)
  ]

is_less_ :: HasValues t => [OpExpr t] -> OpExpr t
is_less_ = binaryOp is_less
is_less :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
is_less = vBinaryOp "is-less" op
  where op vx vy | (Rational x, Rational y) <- (upcastRationals vx
                                             ,upcastRationals vy)
                 = Normal $ inject $ tobool (x < y)
        op _ _ = SortErr "is-less not applied to rationals"

is_less_or_equal_ :: HasValues t => [OpExpr t] -> OpExpr t
is_less_or_equal_ = binaryOp is_less_or_equal
is_less_or_equal :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
is_less_or_equal = vBinaryOp "is-less-or-equal" op
  where op vx vy | (Rational x, Rational y) <- (upcastRationals vx
                                             ,upcastRationals vy)
                 = Normal $ inject $ tobool (x <= y)
        op _ _ = SortErr "is-less-or-equal not applied to rationals"


is_greater_ :: HasValues t => [OpExpr t] -> OpExpr t
is_greater_ = binaryOp is_greater
is_greater :: HasValues t => OpExpr t-> OpExpr t -> OpExpr t
is_greater  = vBinaryOp "is-greater" op
  where op vx vy | (Rational x, Rational y) <- (upcastRationals vx
                                             ,upcastRationals vy)
                 = Normal $ inject $ tobool (x > y)
        op _ _  = SortErr "is-greater not applied to rationals"

is_greater_or_equal_ :: HasValues t => [OpExpr t] -> OpExpr t
is_greater_or_equal_ = binaryOp is_greater_or_equal
is_greater_or_equal :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
is_greater_or_equal = vBinaryOp "is-greater-or-equal" op
  where op vx vy | (Rational x, Rational y) <- (upcastRationals vx
                                             ,upcastRationals vy)
                 = Normal $ inject $ tobool (x >= y)
        op _ _  = SortErr "is-greater-or-equal not applied to rationals"



