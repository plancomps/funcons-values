{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Bits where

import Funcons.Operations.Booleans (tobool,frombool)
import Funcons.Operations.Internal
import Control.Monad (join)

import qualified Data.BitVector as BV

library :: (HasValues t, Ord t) => Library t
library = libFromList [
        ("bit-vector-not", UnaryExpr bit_vector_not)
    ,   ("bit-vector-and", BinaryExpr bit_vector_and)
    ,   ("bit-vector-or", BinaryExpr bit_vector_or)
    ,   ("bit-vector-xor", BinaryExpr bit_vector_xor)
    ,   ("bit-vector-shift-left", BinaryExpr bit_vector_shift_left)
    ,   ("bit-vector-logical-shift-right", BinaryExpr bit_vector_logical_shift_right)
    ,   ("bit-vector-arithmetic-shift-right", BinaryExpr bit_vector_arithmetical_shift_right)
    ,   ("integer-to-bit-vector", BinaryExpr integer_to_bit_vector)
    ,   ("bit-vector-to-integer", UnaryExpr bit_vector_to_integer)
    ,   ("bit-vector-to-natural", UnaryExpr bit_vector_to_natural)
    ]

bit_vector_not_ :: HasValues t => [OpExpr t] -> OpExpr t
bit_vector_not_ = unaryOp bit_vector_not 
bit_vector_not :: HasValues t => OpExpr t -> OpExpr t
bit_vector_not = vUnaryOp "bit-vector-not" op
  where op (ADTVal "bit-vector" vals) | Just bv <- apply_to_vec BV.not vals = 
          Normal $ inject $ ADTVal "bit-vector" bv
        op _ = SortErr "bit-vector-not applied to a bit-vector" 

bit_vector_and_ :: HasValues t => [OpExpr t] -> OpExpr t
bit_vector_and_ = binaryOp bit_vector_and 
bit_vector_and :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
bit_vector_and = vBinaryOp "bit-vector-and" op
  where op (ADTVal "bit-vector" vals1) (ADTVal "bit-vector" vals2) = 
          case args_to_bools vals1 of 
            Just bv1 -> case apply_to_vec (\bv2 -> BV.and [BV.fromBits bv1,bv2]) vals2 of
                          Just bv2 -> Normal $ inject $ ADTVal "bit-vector" bv2
                          Nothing -> SortErr "second argument of bit-vector-and not a bit-vector"
            Nothing -> SortErr "first argument of bit-vector-and not a bit-vector"
        op _ _ = SortErr "bit-vector-and not applied to two bit-vectors" 

bit_vector_or_ :: HasValues t => [OpExpr t] -> OpExpr t
bit_vector_or_ = binaryOp bit_vector_or 
bit_vector_or :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
bit_vector_or = vBinaryOp "bit-vector-or" op
  where op (ADTVal "bit-vector" vals1) (ADTVal "bit-vector" vals2) = 
          case args_to_bools vals1 of 
            Just bv1 -> case apply_to_vec (\bv2 -> BV.or [BV.fromBits bv1,bv2]) vals2 of
                          Just bv2 -> Normal $ inject $ ADTVal "bit-vector" bv2
                          Nothing -> SortErr "second argument of bit-vector-or not a bit-vector"
            Nothing -> SortErr "first argument of bit-vector-or not a bit-vector"
        op _ _ = SortErr "bit-vector-or not applied to two bit-vectors" 

bit_vector_xor_ :: HasValues t => [OpExpr t] -> OpExpr t
bit_vector_xor_ = binaryOp bit_vector_xor 
bit_vector_xor :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
bit_vector_xor = vBinaryOp "bit-vector-xor" (binary_bit_op "bit-vector-xor" BV.xor)

bit_vector_shift_left_ :: HasValues t => [OpExpr t] -> OpExpr t
bit_vector_shift_left_ = binaryOp bit_vector_shift_left 
bit_vector_shift_left :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
bit_vector_shift_left = vBinaryOp "bit-vector-shift-left" (bit_nat_op "bit-vector-shift-left" BV.shl)

bit_vector_logical_shift_right_ :: HasValues t => [OpExpr t] -> OpExpr t
bit_vector_logical_shift_right_ = binaryOp bit_vector_logical_shift_right 
bit_vector_logical_shift_right :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
bit_vector_logical_shift_right = vBinaryOp "bit-vector-logical-shift-right" (bit_nat_op "bit-vector-logical-shift-right" BV.shr)

bit_vector_arithmetical_shift_right_ :: HasValues t => [OpExpr t] -> OpExpr t
bit_vector_arithmetical_shift_right_ = binaryOp bit_vector_arithmetical_shift_right 
bit_vector_arithmetical_shift_right :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
bit_vector_arithmetical_shift_right = vBinaryOp "bit-vector-arithmetical-shift-right" (bit_nat_op "bit-vector-arithmetical-shift-right" BV.ashr)

bit_vector_to_integer_ :: HasValues t => [OpExpr t] -> OpExpr t
bit_vector_to_integer_ = unaryOp bit_vector_to_integer
bit_vector_to_integer :: HasValues t => OpExpr t -> OpExpr t
bit_vector_to_integer = vUnaryOp "bit-vector-to-integer" op
  where op (ADTVal "bit-vector" vals) | Just bits <- args_to_bools vals 
          = Normal $ inject $ Int $ BV.int (BV.fromBits bits)
        op _ = SortErr "bit-vector-to-integer not applied to a bit-vector"  

bit_vector_to_natural_ :: HasValues t => [OpExpr t] -> OpExpr t
bit_vector_to_natural_ = unaryOp bit_vector_to_natural
bit_vector_to_natural :: HasValues t => OpExpr t -> OpExpr t
bit_vector_to_natural = vUnaryOp "bit-vector-to-natural" op
  where op (ADTVal "bit-vector" vals) | Just bits <- args_to_bools vals 
          = Normal $ inject $ Nat $ BV.nat (BV.fromBits bits)
        op _ = SortErr "bit-vector-to-natural not applied to a bit-vector"  

integer_to_bit_vector_ :: HasValues t => [OpExpr t] -> OpExpr t
integer_to_bit_vector_ = binaryOp integer_to_bit_vector
integer_to_bit_vector :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
integer_to_bit_vector = vBinaryOp "integer-to-bit-vector" op
  where op mi mn | Int i <- upcastIntegers mi, Nat n <- upcastNaturals mn 
                 = Normal $ inject $ ADTVal "bit-vector" $ map (inject . tobool) $
                    BV.toBits (BV.bitVec (fromInteger n) (i `mod` (2 ^ n)))
        op _ _ = SortErr "integer-to-bit-vector not applied to an integer and a natural"  
--- lib

apply_to_vec :: HasValues t => (BV.BV -> BV.BV) -> [t] -> Maybe [t]
apply_to_vec app =
  fmap (map (inject . tobool) . BV.toBits . app . BV.fromBits) . args_to_bools

args_to_bools :: HasValues t => [t] -> Maybe [Bool]
args_to_bools = sequence . map (join . fmap frombool . project)

binary_bit_op fc app (ADTVal "bit-vector" vals1) (ADTVal "bit-vector" vals2) = 
          case args_to_bools vals1 of 
            Just bv1 -> case apply_to_vec (app (BV.fromBits bv1)) vals2 of
                          Just bv2 -> Normal $ inject $ ADTVal "bit-vector" bv2
                          Nothing -> SortErr ("second argument of " ++ fc ++ " not a bit-vector")
            Nothing -> SortErr ("first argument of " ++ fc ++ " not a bit-vector")
binary_bit_op fc _ _ _ = SortErr (fc ++ " not applied to two bit-vectors")

bit_nat_op fc app (ADTVal "bit-vector" vals1) v2 | Nat n <- upcastNaturals v2 = 
          case apply_to_vec (flip app (fromInteger n)) vals1 of
            Just bv2 -> Normal $ inject $ ADTVal "bit-vector" bv2
            Nothing -> SortErr ("first argument of " ++ fc ++ " not a bit-vector")
bit_nat_op fc _ _ _ = SortErr (fc ++ " not applied to a bit-vector and a natural")

