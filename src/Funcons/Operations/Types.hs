{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Funcons.Operations.Types where

import Prelude hiding (null)

import Funcons.Operations.Booleans
import Funcons.Operations.Internal
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import qualified Data.Vector as V

library :: (HasValues t, Ord t) => Library t
library = libFromList [
    ("types", NullaryExpr types)
  , ("value-types", NullaryExpr value_types)
  , ("empty-type", NullaryExpr empty_type)
--  , ("null-type", NullaryExpr nulltype)
--  , ("null", NullaryExpr null)
  , ("values", NullaryExpr values)
  , ("type-member", BinaryExpr type_member)
--  , ("is-value", UnaryExpr is_value)
--  , ("is-val", UnaryExpr is_value)
  , ("value-type", UnaryExpr value_type)
  , ("datatype-values", NullaryExpr datatype_values)
  , ("ground-values", NullaryExpr ground_values)
  ]

datatype_values_ :: HasValues t => [OpExpr t] -> OpExpr t
datatype_values_ = nullaryOp datatype_values
datatype_values :: HasValues t => OpExpr t
datatype_values = vNullaryOp "datatype-values" (Normal $ injectT ADTs)

ground_values_ :: HasValues t => [OpExpr t] -> OpExpr t
ground_values_ = nullaryOp ground_values
ground_values :: HasValues t => OpExpr t
ground_values = vNullaryOp "ground-values" (Normal $ injectT (ADT "ground-values" []))

types_ :: HasValues t => [OpExpr t] -> OpExpr t
types_ = nullaryOp types
types :: HasValues t => OpExpr t
types = NullaryOp "types" (Normal $ injectT Types)

value_types_ :: HasValues t => [OpExpr t] -> OpExpr t
value_types_ = nullaryOp value_types
value_types :: HasValues t => OpExpr t
value_types = NullaryOp "value-types" (Normal $ injectT Types)

empty_type_ :: HasValues t => [OpExpr t] -> OpExpr t
empty_type_ = nullaryOp empty_type
empty_type :: HasValues t => OpExpr t
empty_type = NullaryOp "empty-types" (Normal $ injectT EmptyType)

nulltype_ :: HasValues t => [OpExpr t] -> OpExpr t
nulltype_ = nullaryOp nulltype 
nulltype :: HasValues t => OpExpr t
nulltype = NullaryOp "null-type" (Normal $ injectT NullType)

null_ :: HasValues t => [OpExpr t] -> OpExpr t
null_ = nullaryOp null
null :: HasValues t => OpExpr t
null = NullaryOp "null" (Normal $ inject null__)

values_ :: HasValues t => [OpExpr t] -> OpExpr t
values_ = nullaryOp values
values :: HasValues t => OpExpr t
values = NullaryOp "values" (Normal $ injectT Values)


is_value_ :: HasValues t => [OpExpr t] -> OpExpr t
is_value_ = unaryOp is_value
is_value :: HasValues t => OpExpr t -> OpExpr t
is_value = UnaryOp "is-value" op
  where op _ = Normal $ inject (tobool True) 

value_type_ :: HasValues t => [OpExpr t] -> OpExpr t
value_type_ = unaryOp value_type
value_type :: HasValues t => OpExpr t -> OpExpr t
value_type = vUnaryOp "value-type" (Normal . injectT . tyOf)
 
tyOf :: HasValues t => Values t -> Types t
tyOf (ADTVal "true" [])         = ADT "booleans" []
tyOf (ADTVal "false" [])        = ADT "booleans" []
tyOf (ADTVal c [p]) | c == unicode_cons = UnicodeCharacters
tyOf (Int _)                    = Integers
tyOf (Nat _)                    = Naturals
tyOf (ADTVal _ _)               = ADTs
tyOf (Atom _)                   = Atoms
tyOf (ComputationType (Type _)) = Types
tyOf (ComputationType _)        = ComputationTypes
tyOf (Float f)                  = IEEEFloats Binary32 
--tyOf (Type _)                   = Types 
tyOf (IEEE_Float_32 _)          = IEEEFloats Binary32 
tyOf (IEEE_Float_64 _)          = IEEEFloats Binary64 
tyOf (Rational _)               = Rationals
tyOf (Map m)                    = maps (injectT Values) (injectT Values) -- TODO find "strongest common type"
tyOf (Set s)                    | S.null s = sets Values
                                | otherwise = sets (tyOf (S.findMax s))
tyOf (Multiset s)               | MS.null s = multisets Values
                                | otherwise = multisets (tyOf (MS.findMax s)) 
tyOf (Vector v)                 | V.null v = vectors Values
                                | otherwise = vectors (tyOf (v V.! 0))
tyOf VAny                       = Values
tyOf (ValSeq ts)                = Values

type_member_ :: HasValues t => [OpExpr t] -> OpExpr t
type_member_ = binaryOp type_member
-- | Type membership check for primitive types and
-- predefined composite types (non-ADTs).
type_member :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
type_member = vBinaryOp "type-member" op
  where op v mty = case mty of
         ComputationType (Type t) -> proceed v t
         ComputationType (ComputesType t) -> proceed v t
         ComputationType (ComputesFromType _ t) -> proceed v t
         _ -> SortErr "type-member(V,Ty) not applied to a value and a type"
 
        proceed v ty = case isInType v ty of
          Nothing -> DomErr "type-member applied to an ADT or a non-type"
          Just b  -> Normal $ inject (tobool b)

isInType :: HasValues t => Values t -> Types t -> Maybe Bool
isInType _ EmptyType = return False
isInType v Values = return True --(not (isNull v)) 
isInType n NullType = return (isNull n) 
isInType v (ADT "ground-values" []) = return (isGround v)
isInType v (ADT "strings" []) = return (isString_ v)
isInType (ADTVal "list" vs') (ADT "lists" [ty']) 
  | Just ty <- projectT ty', Just vs <- sequence (map project vs') = 
  and <$> mapM (flip isInType ty) vs

isInType v (ADT nm tys) = Nothing
isInType (ADTVal _ _) ADTs = return True
isInType (Atom _) Atoms = return True
isInType v Characters | Just _ <- upcastCharacter v = return True
isInType v (IntegersFrom n) 
    | Int i <- upcastIntegers v = return (i >= n)
isInType v (IntegersUpTo n) 
    | Int i <- upcastIntegers v = return (i <= n)
isInType (ComputationType _) Types = return True
isInType (ComputationType (ComputesFromType _ _)) ComputationTypes = return True
isInType (ComputationType (ComputesType _)) ComputationTypes = return True
isInType (ComputationType (Type _)) ComputationTypes = return True
isInType (IEEE_Float_32 _) (IEEEFloats Binary32) = return True
isInType (IEEE_Float_64 _) (IEEEFloats Binary64) = return True
isInType v Integers | Int _ <- upcastIntegers v = return True
isInType v Naturals | Nat _ <- upcastNaturals v = return True
isInType v Rationals | Rational _ <- upcastRationals v = return True 
isInType (ADTVal c [p]) UnicodeCharacters |  c == unicode_cons = return True
isInType v AsciiCharacters = isInType v UnicodeCharacters -- requires interpreter to check whether character is in the character range 
isInType v ISOLatinCharacters = isInType v UnicodeCharacters -- requires interpreter to check whether character is in the character range 
isInType v BMPCharacters = isInType v UnicodeCharacters -- requires interpreter to check whether character is in the character range 
isInType v (Union ty1 ty2) = (||) <$> isInType v ty1 <*> isInType v ty2
isInType v (Complement ty) = not <$> isInType v ty
isInType v (Intersection ty1 ty2) = (&&) <$> isInType v ty1 <*> isInType v ty2
isInType _ _ = return False

isInTupleType :: HasValues t => [Values t] -> [Types t] -> Maybe Bool
isInTupleType vs ttparams = and <$> sequence (zipWith isInType vs ttparams)
