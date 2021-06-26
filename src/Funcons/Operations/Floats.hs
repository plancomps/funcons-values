{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Floats where

import Funcons.Operations.Internal hiding (isInt)

library :: HasValues t => Library t
library = libFromList [
  ]

ieee_float_truncate_ :: HasValues t => [OpExpr t] -> OpExpr t
ieee_float_truncate_ = binaryOp ieee_float_truncate 
ieee_float_truncate :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
ieee_float_truncate = vBinaryOp "ieee-float-truncate" op
  where op (IEEE_Float_64 f) (ADTVal "binary64" _) = Normal $ inject $ Int (truncate f)
        op _ _ = SortErr "ieee-float-truncate not applied to a float of the right format"

ieee_float_add_ :: HasValues t => [OpExpr t] -> OpExpr t
ieee_float_add_ = vNaryOp "ieee-float-add" op
  where op (format:vs) = ieee_float_op "ieee-float-add" (+) 0 format vs
        op [] = SortErr "ieee-float-add not applied to a format and a list of floats"

{-
ieee_float_multiply_ = ieee_float_multiply 
ieee_float_multiply = applyFuncon "ieee-float-multiply"
ieee_float_multiply_op (format:vs) = ieee_float_op "ieee_float-multiply" ieee_float_multiply (*) 1 format vs
ieee_float_multiply_op [] = sortErr (ieee_float_multiply [listval []]) "ieee-float-multiply not applied to a format and a list of floats"

ieee_float_divide = applyFuncon "ieee-float-divide"
ieee_float_divide_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ IEEE_Float_64 $ (f1 / f2)
ieee_float_divide_op ft vx vy = sortErr (ieee_float_divide [FValue ft,FValue vx, FValue vy])
                         "ieee-float-divide not applied to a format and ieee-floats"

ieee_float_remainder = applyFuncon "ieee-float-remainder"
ieee_float_remainder_op format vx vy
    | isIEEEFormat format vx =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ IEEE_Float_64 $ (f1 `mod'` f2)
ieee_float_remainder_op ft vx vy = sortErr (ieee_float_remainder [FValue ft,FValue vx, FValue vy])
                         "ieee-float-remainder not applied to a format and ieee-floats"

ieee_float_negate = applyFuncon "ieee-float-negate"
ieee_float_negate_op format vx
    | isIEEEFormat format vx = let f1 = doubleFromIEEEFormat format vx
                               in rewriteTo $ FValue $ IEEE_Float_64 (-f1)
    | otherwise = sortErr (ieee_float_negate [FValue format,FValue vx]) "ieee-float-negate not applied to ieee-float"

ieee_float_subtract = applyFuncon "ieee-float-subtract"
ieee_float_subtract_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ IEEE_Float_64 $ (f1 - f2)
ieee_float_subtract_op ft vx vy = sortErr (ieee_float_subtract [FValue ft, FValue vx, FValue vy])
                         "ieee-float-subtract not applied to a format and ieee-floats"

ieee_float_float_power = applyFuncon "ieee-float-float-power"
ieee_float_power_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ IEEE_Float_64 $ (f1 ** f2)
ieee_float_power_op ft vx vy = sortErr (ieee_float_float_power [FValue ft, FValue vx, FValue vy])
                         "ieee-float-power not applied to a format and ieee-floats"

ieee_float_is_less = applyFuncon "ieee-float-is-less"
ieee_float_is_less_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ tobool (f1 < f2)
ieee_float_is_less_op ft vx vy = sortErr (ieee_float_is_less [FValue ft, FValue vx, FValue vy])
                         "ieee-float-is-less not applied to a format and ieee-floats"

ieee_float_is_greater = applyFuncon "ieee-float-is-greater"
ieee_float_is_greater_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ tobool (f1 > f2)
ieee_float_is_greater_op ft vx vy = sortErr (ieee_float_is_greater [FValue ft, FValue vx, FValue vy])
                         "ieee-float-is-greater not applied to a format and ieee-floats"

ieee_float_is_less_or_equal = applyFuncon "ieee-float-is-less-or-equal"
ieee_float_is_less_or_equal_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ tobool (f1 <= f2)
ieee_float_is_less_or_equal_op ft vx vy = sortErr (ieee_float_is_less_or_equal [FValue ft,FValue vx, FValue vy])
                         "ieee-float-is-less-or-equal not applied to a format and ieee-floats"

ieee_float_is_greater_or_equal = applyFuncon "ieee-float-is-greater-or-equal"
ieee_float_is_greater_or_equal_op format vx vy
    | isIEEEFormat format vx && isIEEEFormat format vy =
        let f1 = doubleFromIEEEFormat format vx
            f2 = doubleFromIEEEFormat format vy
        in rewriteTo $ FValue $ tobool (f1 >= f2)
ieee_float_is_greater_or_equal_op ft vx vy = sortErr (ieee_float_is_greater_or_equal [FValue ft,FValue vx, FValue vy])
                         "ieee-float-is-greater-or-equal not applied to a format and ieee-floats"


signed_bits_maximum = applyFuncon "signed-bits-maximum"
stepSigned_Bits_Maximum [vn] | Nat n <- upcastNaturals vn
        = rewriteTo $ integer_subtract_ [integer_power_ [int_ 2, integer_subtract_ [int_ $ fromInteger n, int_ 1]],int_ 1]
stepSigned_Bits_Maximum vs = sortErr (signed_bits_maximum (fvalues vs)) "sort check"

signed_bits_minimum = applyFuncon "signed-bits-minimum"
stepSigned_Bits_Minimum [vn] | Nat n <- upcastNaturals vn
        = rewriteTo $ applyFuncon "integer-negate" [signed_bits_maximum [FValue vn]]
stepSigned_Bits_Minimum vs = sortErr (signed_bits_maximum (fvalues vs)) "sort check"

    -- TODO binary64 assumption (perhaps use config files)
ieee_float_acos = applyFuncon "ieee-float-acos"
stepIEEE_Float_Acos [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (acos f1)
stepIEEE_Float_Acos vn = sortErr (ieee_float_acos (fvalues vn)) "sort check"

ieee_float_asin = applyFuncon "ieee-float-asin"
stepIEEE_Float_Asin [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (asin f1)
stepIEEE_Float_Asin vn = sortErr (ieee_float_asin (fvalues vn)) "sort check"

ieee_float_atan = applyFuncon "ieee-float-atan"
stepIEEE_Float_Atan [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (atan f1)
stepIEEE_Float_Atan vn = sortErr (ieee_float_atan (fvalues vn)) "sort check"

ieee_float_atan2 = applyFuncon "ieee-float-atan2"
stepIEEE_Float_Atan2 [f,vx,vy] = let f1 = doubleFromIEEEFormat f vx
                                     f2 = doubleFromIEEEFormat f vy
                                 in rewriteTo $ FValue $ IEEE_Float_64 (atan2 f1 f2)
stepIEEE_Float_Atan2 vn = sortErr (ieee_float_atan2 (fvalues vn)) "sort check"

ieee_float_cos = applyFuncon "ieee-float-cos"
stepIEEE_Float_Cos [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (cos f1)
stepIEEE_Float_Cos vn = sortErr (ieee_float_cos (fvalues vn)) "sort check"

ieee_float_cosh = applyFuncon "ieee-float-cosh"
stepIEEE_Float_Cosh [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (cosh f1)
stepIEEE_Float_Cosh vn = sortErr (ieee_float_cosh (fvalues vn)) "sort check"

ieee_float_exp = applyFuncon "ieee-float-exp"
stepIEEE_Float_Exp [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (exp f1)
stepIEEE_Float_Exp vn = sortErr (ieee_float_exp (fvalues vn)) "sort check"

ieee_float_log = applyFuncon "ieee-float-log"
stepIEEE_Float_Log [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (log f1)
stepIEEE_Float_Log vn = sortErr (ieee_float_log (fvalues vn)) "sort check"

ieee_float_log10 = applyFuncon "ieee-float-log10"
stepIEEE_Float_Log10 [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (logBase 10 f1)
stepIEEE_Float_Log10 vn = sortErr (ieee_float_log10 (fvalues vn)) "sort check"

ieee_float_sin = applyFuncon "ieee-float-sin"
stepIEEE_Float_Sin [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (sin f1)
stepIEEE_Float_Sin vn = sortErr (ieee_float_sin (fvalues vn)) "sort check"

ieee_float_sinh = applyFuncon "ieee-float-sinh"
stepIEEE_Float_Sinh [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (sinh f1)
stepIEEE_Float_Sinh vn = sortErr (ieee_float_sinh (fvalues vn)) "sort check"

ieee_float_sqrt = applyFuncon "ieee-float-sqrt"
stepIEEE_Float_Sqrt [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (sqrt f1)
stepIEEE_Float_Sqrt vn = sortErr (ieee_float_sqrt (fvalues vn)) "sort check"

ieee_float_tan = applyFuncon "ieee-float-tan"
stepIEEE_Float_Tan [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (tan f1)
stepIEEE_Float_Tan vn = sortErr (ieee_float_tan (fvalues vn)) "sort check"

ieee_float_tanh = applyFuncon "ieee-float-tanh"
stepIEEE_Float_Tanh [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (tanh f1)
stepIEEE_Float_Tanh vn = sortErr (ieee_float_tanh (fvalues vn)) "sort check"

ieee_float_ceiling = applyFuncon "ieee-float-ceiling"
stepIEEE_Float_Ceiling [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ int_ (ceiling f1)
stepIEEE_Float_Ceiling vn = sortErr (ieee_float_ceiling (fvalues vn)) "sort check"

ieee_float_floor = applyFuncon "ieee-float-floor"
stepIEEE_Float_Floor [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ int_ (floor f1)
stepIEEE_Float_Floor vn = sortErr (ieee_float_floor (fvalues vn)) "sort check"

ieee_float_absolute_value = applyFuncon "ieee-float-absolute-value"
stepIEEE_Float_Absolute_Value [f,vx] = let f1 = doubleFromIEEEFormat f vx
                                     in rewriteTo $ FValue $ IEEE_Float_64 (Prelude.abs f1)
stepIEEE_Float_Absolute_Value vn = sortErr (ieee_float_absolute_value (fvalues vn)) "sort check"

stepIEEE_Float_Remainder [f,f1,f2] = ieee_float_remainder_op f f1 f2
stepIEEE_Float_Remainder vn = sortErr (ieee_float_remainder (fvalues vn)) "sort check"

stepIEEE_Float_Is_Less [f,f1,f2] = ieee_float_is_less_op f f1 f2
stepIEEE_Float_Is_Less vn = sortErr (ieee_float_is_less (fvalues vn)) "sort check"
stepIEEE_Float_Is_Greater [f,f1,f2] = ieee_float_is_greater_op f f1 f2
stepIEEE_Float_Is_Greater vn = sortErr (ieee_float_is_greater (fvalues vn)) "sort check"
stepIEEE_Float_Is_Less_Or_Equal [f,f1,f2] = ieee_float_is_less_or_equal_op f f1 f2
stepIEEE_Float_Is_Less_Or_Equal vn = sortErr (ieee_float_is_less_or_equal (fvalues vn)) "sort check"
stepIEEE_Float_Is_Greater_Or_Equal [f,f1,f2] = ieee_float_is_greater_or_equal_op f f1 f2
stepIEEE_Float_Is_Greater_Or_Equal vn = sortErr (ieee_float_is_greater_or_equal (fvalues vn)) "sort check"
-}

ieee_float_op :: HasValues t => String -> (Double -> Double -> Double) 
                -> Double -> Values t -> [Values t] -> Result t 
ieee_float_op str f b format vs
    | all (isIEEEFormat format) vs = Normal $ inject $ IEEE_Float_64
        $ foldr f b $ map (doubleFromIEEEFormat format) vs
    | otherwise = SortErr err
    where   err     = str ++ " not applied to ieee_floats"


isIEEEFormat :: Values t -> Values t -> Bool
isIEEEFormat (ADTVal "binary32" _) (IEEE_Float_32 _) = True
isIEEEFormat (ADTVal "binary64" _) (IEEE_Float_64 _) = True
isIEEEFormat _ _ = False

doubleFromIEEEFormat :: Values t -> Values t -> Double
doubleFromIEEEFormat (ADTVal "binary64" _) (IEEE_Float_64 d) = d
doubleFromIEEEFormat _ _ = error "fromIEEEFormat"


