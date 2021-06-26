{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.ADTs where

import Funcons.Operations.Internal

import Data.Text (pack, unpack)
import Data.String(fromString)

library :: HasValues t => Ord t => Library t
library = libFromList [
    ("adts", NullaryExpr adts)
  , ("adt-construct", NaryExpr adt_construct_)
  , ("adt-type", NaryExpr adt_type_construct_)
  , ("adt-type-construct", NaryExpr adt_type_construct_)
  , ("adt-constructor", UnaryExpr adt_constructor)
  , ("adt-fields", UnaryExpr adt_fields)
  ]

adts_ :: HasValues t => [OpExpr t] -> OpExpr t
adts_ = nullaryOp adts
adts :: HasValues t => OpExpr t
adts = NullaryOp "adts" (Normal $ injectT ADTs)

adt_construct_ :: HasValues t => [OpExpr t] -> OpExpr t
adt_construct_ = NaryOp "adt-construct" op
  where op :: HasValues t => [t] -> Result t
        op (x : vs) = case project x of
          Just v -> 
            if isString_ v 
              then Normal $ inject $ ADTVal (pack (unString v)) vs
              else SortErr "adt-construct: first argument not a string"
          _      -> ProjErr "adt-construct"
        op _  = SortErr "adt-construct: insufficient arguments"

adt_type_construct_ :: HasValues t => [OpExpr t] -> OpExpr t
adt_type_construct_ = vNaryOp "adt-type-construct" op
  where op (s : vs) 
          | isString_ s = Normal $ injectT $ ADT (pack (unString s))
                            (map inject vs)
        op _  = SortErr "adt-construct: first argument not a string"

adt_constructor_ :: HasValues t => [OpExpr t] -> OpExpr t
adt_constructor_ = unaryOp adt_constructor
adt_constructor :: HasValues t => OpExpr t -> OpExpr t
adt_constructor = vUnaryOp "adt-constructor" op
  where op (ADTVal cons _) = Normal $ inject $ fromString (unpack cons)
        op _ = SortErr "adt-constructor: argument not an adt value"

adt_fields_ :: HasValues t => [OpExpr t] -> OpExpr t
adt_fields_ = unaryOp adt_fields
adt_fields :: HasValues t => OpExpr t -> OpExpr t
adt_fields = vUnaryOp "adt-fields" op
  where op (ADTVal _ fs) = Normal $ inject $ ADTVal "list" fs 
        op _ = SortErr "adt-fields: argument not an adt value"
