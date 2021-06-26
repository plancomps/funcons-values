{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Integers where

import Funcons.Operations.Internal hiding (isInt)
import Funcons.Operations.Types
import Funcons.Operations.Booleans (tobool)

import Data.Char 
import Numeric

library :: HasValues t => Library t
library = libFromList [
    ("integers", NullaryExpr integers)
  , ("integers-from", UnaryExpr integers_from)
  , ("from", UnaryExpr integers_from)
  , ("integers-up-to", UnaryExpr integers_up_to)
  , ("up-to", UnaryExpr integers_up_to)
  , ("is-integer", UnaryExpr is_integer)
  , ("is-int", UnaryExpr is_integer)
  , ("int-add", NaryExpr integer_add_)
  , ("integer-add", NaryExpr integer_add_)
  , ("int-sub", BinaryExpr integer_subtract)
  , ("integer-subtract", BinaryExpr integer_subtract)
  , ("integer-sub", BinaryExpr integer_subtract)
  , ("integer-modulo", BinaryExpr stepMod)
  , ("integer-mod", BinaryExpr stepMod)
  , ("int-mod", BinaryExpr stepMod)
  , ("integer-multiply", NaryExpr integer_multiply_)
  , ("int-mul", NaryExpr integer_multiply_)
  , ("integer-divide", BinaryExpr integer_divide)
  , ("int-div", BinaryExpr integer_divide)
  , ("integer-power", BinaryExpr integer_power)
  , ("int-pow", BinaryExpr integer_power)
-- integer-negate is now generated
--  ,   ("integer-negate", ValueOp stepInteger_Negate)
--  ,   ("int-neg", ValueOp stepInteger_Negate)
  , ("integer-list", BinaryExpr integer_list)
  , ("integer-absolute-value", UnaryExpr integer_absolute_value)
  , ("decimal-natural", UnaryExpr decimal_natural)
  , ("decimal", UnaryExpr decimal_natural)
  , ("hexadecimal-natural", UnaryExpr hexadecimal_natural)
  , ("hexadecimal", UnaryExpr hexadecimal_natural)
  , ("binary-natural", UnaryExpr binary_natural)
  , ("binary", UnaryExpr binary_natural)
  , ("octal-natural", UnaryExpr octal_natural)
  , ("octal", UnaryExpr octal_natural)
  , ("natural-predecessor", UnaryExpr natural_predecessor)
  , ("nat-pred", UnaryExpr natural_predecessor)
  , ("natural-successor", UnaryExpr natural_successor)
  , ("nat-successor", UnaryExpr natural_successor)
  , ("nat-succ", UnaryExpr natural_successor)
  , ("integer-is-less", BinaryExpr is_less)
  , ("is-less", BinaryExpr is_less)
  , ("is-less-or-equal", BinaryExpr is_less_or_equal)
  , ("integer-is-less-or-equal", BinaryExpr is_less_or_equal)
  , ("is-greater", BinaryExpr is_greater)
  , ("integer-is-greater", BinaryExpr is_greater)
  , ("is-greater-or-equal", BinaryExpr is_greater_or_equal)
  , ("integer-is-greater-or-equal", BinaryExpr is_greater_or_equal)
  ]

integer_modulo_, integer_mod_ :: HasValues t => [OpExpr t] -> OpExpr t
integer_mod_ = binaryOp stepMod
integer_modulo_ = binaryOp stepMod

stepMod :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
stepMod = vBinaryOp "mod" op
  where op vx vy | (Int x, Int y)<- (upcastIntegers vx, upcastIntegers vy)
                 = if y == 0 then Normal $ inject null__ 
                             else Normal $ inject $ mk_integers $ x `mod` y
        op _ _ = SortErr "mod not applied to integers"

integers_ :: HasValues t => [OpExpr t] -> OpExpr t
integers_ = nullaryOp integers
integers :: HasValues t => OpExpr t
integers = vNullaryOp "integers" (Normal $ injectT Integers)

integers_from_ :: HasValues t => [OpExpr t] -> OpExpr t
integers_from_ = unaryOp integers_from
integers_from :: HasValues t => OpExpr t -> OpExpr t
integers_from = vUnaryOp "integers-from" op
  where op v | Int i <- upcastIntegers v = Normal $ injectT $ IntegersFrom i
             | otherwise = SortErr "integers-from not applied to an integer"

integers_up_to_ :: HasValues t => [OpExpr t] -> OpExpr t
integers_up_to_ = unaryOp integers_up_to
integers_up_to :: HasValues t => OpExpr t -> OpExpr t
integers_up_to = vUnaryOp "integers-up-to" op
  where op v | Int i <- upcastIntegers v = Normal $ injectT $ IntegersUpTo i
             | otherwise = SortErr "integers-up-to not applied to an integer"

is_integer_ :: HasValues t => [OpExpr t] -> OpExpr t
is_integer_ = unaryOp is_integer
is_integer :: HasValues t => OpExpr t -> OpExpr t
is_integer x = RewritesTo "is-integer" (type_member x (ValExpr (ComputationType (Type Integers)))) [x]

isInt :: Values t -> Bool
isInt x | Int i <- upcastIntegers x = True 
        | otherwise = False

unInt x | Int i <- upcastIntegers x = i
        | otherwise = error "unInt"

integer_add_ :: HasValues t => [OpExpr t] -> OpExpr t
integer_add_ = vNaryOp "integer-add" op
  where op  xs | all isInt xs = Normal $ inject $ mk_integers $ sum (map unInt xs)
               | otherwise = SortErr "integer-add not applied to integers"

integer_multiply_ :: HasValues t => [OpExpr t] -> OpExpr t
integer_multiply_ = vNaryOp "integer-multiply" op
  where op  xs | all isInt xs = Normal $ inject $ mk_integers $ product (map unInt xs)
               | otherwise = SortErr "integer-multiply not applied to integers"


integer_subtract_ :: HasValues t => [OpExpr t] -> OpExpr t
integer_subtract_ = binaryOp integer_subtract
integer_subtract :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
integer_subtract = vBinaryOp "integer-subtract" op
  where op vx vy | Int x <- upcastIntegers vx
                 , Int y <- upcastIntegers vy = Normal $ inject $ mk_integers $ (x - y)
        op _ _ = SortErr "integer-subtract not applied to integers"

integer_divide_ :: HasValues t => [OpExpr t] -> OpExpr t
integer_divide_ = binaryOp integer_divide
integer_divide :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
integer_divide = vBinaryOp "integer-divide" op
  where op vx vy
          | (Int x,Int y) <- (upcastIntegers vx, upcastIntegers vy) = 
              if y == 0 
              then Normal $ inject null__
              else Normal $ inject $ mk_integers $ fromInteger (x `div` y)
          | otherwise = SortErr "integer-divide not applied to ints" 

integer_power_ :: HasValues t => [OpExpr t] -> OpExpr t
integer_power_ = binaryOp integer_power  
integer_power :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
integer_power = vBinaryOp "integer-power" op
  where op vx vy
          | (Int x, Int y) <- (upcastIntegers vx, upcastIntegers vy) = 
              Normal $ inject $ mk_integers $ fromInteger $(x ^ y)
          | otherwise = SortErr "integer-power not applied to two integers"

natural_predecessor_, nat_pred_ :: HasValues t => [OpExpr t] -> OpExpr t
natural_predecessor_ = unaryOp natural_predecessor 
nat_pred_ = unaryOp natural_predecessor 
natural_predecessor :: HasValues t => OpExpr t -> OpExpr t
natural_predecessor = vUnaryOp "natural-predecessor" op
  where op x | Nat n <- upcastNaturals x =
          if n == 0 then Normal $ inject null__ 
                    else Normal $ inject $ Nat (n - 1) 
             | otherwise = SortErr "natural-pred not applied to a natural number"

natural_successor_, nat_succ_ :: HasValues t => [OpExpr t] -> OpExpr t
natural_successor_ = nat_succ_
nat_succ_ = unaryOp natural_successor
natural_successor :: HasValues t => OpExpr t -> OpExpr t
natural_successor = vUnaryOp "natural-successor" op
  where op x | Nat n <- upcastNaturals x = Normal $ inject $ Nat (n + 1) 
             | otherwise = SortErr "natural-succ not applied to a natural number"

integer_list_ :: HasValues t => [OpExpr t] -> OpExpr t
integer_list_ = binaryOp integer_list
integer_list :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
integer_list = vBinaryOp "integer-list" op
 where op vi1 vi2
        | (Int i1, Int i2) <- (upcastIntegers vi1, upcastIntegers vi2)
                = Normal $ inject $ ADTVal "list" (map (inject . Int) [i1.. i2])
        | otherwise = SortErr "integer-list not applied to two integers"

integer_absolute_value_ :: HasValues t => [OpExpr t] -> OpExpr t
integer_absolute_value_ = unaryOp integer_absolute_value 
integer_absolute_value :: HasValues t => OpExpr t -> OpExpr t
integer_absolute_value = vUnaryOp "integer-absolute-value" op
  where op v | Int  i <- upcastIntegers v = 
                Normal $ inject $ Int (Prelude.abs i)
             | otherwise = SortErr "sort check: integer-absolute-value(I1)"

decimal_natural_ :: HasValues t => [OpExpr t] -> OpExpr t
decimal_natural_ = unaryOp decimal_natural 
decimal_natural :: HasValues t => OpExpr t -> OpExpr t
decimal_natural = vUnaryOp "decimal-natural" op
  where op :: HasValues t => Values t -> Result t 
        op s | isString_ s = Normal $ inject $ Nat (read (unString s))
             | otherwise = SortErr "decimal-natural not applied to a string"

binary_natural_ :: HasValues t => [OpExpr t] -> OpExpr t
binary_natural_ = unaryOp decimal_natural
binary_natural :: HasValues t => OpExpr t -> OpExpr t
binary_natural = vUnaryOp "binary-natural" op
  where op  :: HasValues t => Values t -> Result t
        op s | isString_ s = case readInt 2 (`elem` ['0'..'1']) digitToInt (unString s) of
                [(i,"")] -> Normal $ inject $ Nat i
                _        -> SortErr "binary-natural not applied to a binary number"
             | otherwise = SortErr "binary-natural not applied to a string"

octal_natural_ :: HasValues t => [OpExpr t] -> OpExpr t
octal_natural_ = unaryOp decimal_natural
octal_natural :: HasValues t => OpExpr t -> OpExpr t
octal_natural = vUnaryOp "octal-natural" op
  where op  :: HasValues t => Values t -> Result t
        op s | isString_ s = case readInt 8 (`elem` ['0'..'7']) digitToInt (unString s) of
                [(i,"")] -> Normal $ inject $ Nat i
                _        -> SortErr "octal-natural not applied to a octal number"
             | otherwise = SortErr "octal-natural not applied to a string"

hexadecimal_natural_ :: HasValues t => [OpExpr t] -> OpExpr t
hexadecimal_natural_ = unaryOp hexadecimal_natural
hexadecimal_natural :: HasValues t => OpExpr t -> OpExpr t
hexadecimal_natural = vUnaryOp "hexadecimal-natural" op
  where op  :: HasValues t => Values t -> Result t
        op s | isString_ s = case readInt 16 (`elem` (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])) digitToInt (unString s) of
                [(i,"")] -> Normal $ inject $ Nat i
                _        -> SortErr "hexadecimal-natural not applied to a hexadecimal number"
             | otherwise = SortErr "hexadecimal-natural not applied to a string"

is_less_ :: HasValues t => [OpExpr t] -> OpExpr t
is_less_ = binaryOp is_less
is_less :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
is_less = vBinaryOp "is-less" op
  where op vx vy
          | (Int x, Int y) <- (upcastIntegers vx, upcastIntegers vy)
              = Normal $ inject $ tobool (x < y)
          | otherwise = SortErr "is-less not applied to rationals"

is_less_or_equal_ :: HasValues t => [OpExpr t] -> OpExpr t
is_less_or_equal_ = binaryOp is_less_or_equal
is_less_or_equal ::  HasValues t => OpExpr t -> OpExpr t -> OpExpr t
is_less_or_equal = vBinaryOp "is-less-or-equal" op
 where op vx vy
         | (Int x, Int y) <- (upcastIntegers vx, upcastIntegers vy)
            = Normal $ inject $ tobool (x <= y) 
         | otherwise = SortErr "is_less_or_equal not applied to two arguments"

is_greater_ :: HasValues t => [OpExpr t] -> OpExpr t
is_greater_ = binaryOp is_greater
is_greater :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
is_greater = vBinaryOp "is-greater" op
 where op vx vy
        | (Int x, Int y) <- (upcastIntegers vx, upcastIntegers vy)
            = Normal $ inject $ tobool (x > y) 
        | otherwise = SortErr "is-greater not applied to two arguments"

is_greater_or_equal_ :: HasValues t => [OpExpr t] -> OpExpr t
is_greater_or_equal_ = binaryOp is_greater_or_equal 
is_greater_or_equal :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
is_greater_or_equal = vBinaryOp "is-greater-or-equal" op
 where op vx vy | (Int x, Int y) <- (upcastIntegers vx, upcastIntegers vy)
                    = Normal $ inject $ tobool (x >= y) 
                | otherwise = SortErr "is-greater-or-equal not applied to rationals"
