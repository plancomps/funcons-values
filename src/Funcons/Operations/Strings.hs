{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Strings where

import Funcons.Operations.Libraries
import Funcons.Operations.Internal
import Funcons.Operations.Types

import Data.String

library :: HasValues t => Library t
library = libFromList [
    ("is-string", UnaryExpr is_string)
  , ("strings", NullaryExpr (vNullaryOp "strings" (Normal $ injectT $ ADT "strings" [])))
  , ("to-string", UnaryExpr to_string)
  ]

is_string_ :: HasValues t => [OpExpr t] -> OpExpr t
is_string_ = unaryOp is_string
is_string x = RewritesTo "is-string" (type_member x (ValExpr (ComputationType (Type (ADT "strings" []))))) [x]

to_string_ :: HasValues t => [OpExpr t] -> OpExpr t
to_string_ = unaryOp to_string
to_string :: HasValues t => OpExpr t -> OpExpr t 
to_string = vUnaryOp "to-string" stepTo_String

stepTo_String s | isString_ s   = Normal $ inject $ s
stepTo_String (Rational r)      = mk_string (show (fromRational r))
stepTo_String v | Just c <- upcastCharacter v = mk_string ([c])
stepTo_String (Atom s)          = mk_string  s
stepTo_String (Int i)           = mk_string  (show i)
stepTo_String (Nat n)           = mk_string  (show n)
stepTo_String (Float f)         = mk_string  (show f)
stepTo_String (IEEE_Float_32 f) = mk_string  (show f)
stepTo_String (IEEE_Float_64 d) = mk_string  (show d)
stepTo_String (ADTVal "true" []) = mk_string "true"
stepTo_String (ADTVal "false"[]) = mk_string "false"
stepTo_String (ADTVal "null"[]) = mk_string "null"
stepTo_String v                 = DomErr ("to-string undefined on this type")

mk_string :: HasValues t => String -> Result t
mk_string = Normal . inject . fromString 
