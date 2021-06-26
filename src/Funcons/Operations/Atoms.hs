{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Atoms where

import Funcons.Operations.Internal

library :: (HasValues t, Eq t) => Library t
library = libFromList [
    ("atoms", NullaryExpr atoms)
  , ("next-atom", UnaryExpr next_atom)
  , ("atom-seed", NullaryExpr atom_seed)
  , ("atom", UnaryExpr atom)
  ]

atom_seed_ :: HasValues t => [OpExpr t] -> OpExpr t
atom_seed_ = nullaryOp atom_seed 
atom_seed :: HasValues t => OpExpr t
atom_seed = vNullaryOp "atom-seed" (Normal $ inject $ Atom "0")

next_atom_ :: HasValues t => [OpExpr t] -> OpExpr t
next_atom_ = unaryOp next_atom
next_atom :: HasValues t => OpExpr t -> OpExpr t
next_atom = vUnaryOp "next-atom" op
  where op (Atom a) = Normal $ inject $ Atom (show (i+1))
          where i::Int
                i = read a
        op _ = SortErr "next-atom not applied to an atom"

atoms_ :: HasValues t => [OpExpr t] -> OpExpr t
atoms_ = nullaryOp atoms
atoms :: HasValues t => OpExpr t
atoms = vNullaryOp "atoms" (Normal $ injectT Atoms)

atom_ :: HasValues t => [OpExpr t] -> OpExpr t
atom_ = unaryOp atom

atom :: HasValues t => OpExpr t -> OpExpr t
atom = vUnaryOp "atom" op
  where op v | isString_ v = Normal $ inject $ Atom (unString v)
             | otherwise   = SortErr "atom not applied to a string"  
