
module Funcons.Operations.Eval where

import Funcons.Operations.Expr
import Funcons.Operations.Values hiding (showArgs)

import Data.List (intercalate)

data EvalResult t = Error         (OpExpr t) (Result t)
                  | Success       t
                  | EvalResults   [EvalResult t]
                  deriving (Show)

eval :: HasValues t => OpExpr t -> EvalResult t
eval expr = applyEval expr (applyExpr expr) 

applyEval :: OpExpr t -> Result t -> EvalResult t
applyEval expr (Normal v) = Success v
applyEval expr (Nondeterministic ress) = EvalResults (map (applyEval expr) ress)
applyEval expr res = Error expr res 
 
instance (HasValues t, Show t) => Show (OpExpr t) where
  show (ValExpr v)            = ppValues show v
  show (TermExpr t)           = show t
  show (NullaryOp nm _)       = nm
  show (UnaryOp nm _ x)       = nm ++ showArgs [x]
  show (BinaryOp nm _ x y)    = nm ++ showArgs [x,y]
  show (TernaryOp nm _ x y z) = nm ++ showArgs [x,y,z]
  show (NaryOp nm _ xs)       = nm ++ showArgs xs
  show (InvalidOp nm _ xs)    = nm ++ showArgs xs
  show (RewritesTo nm _ xs)   = nm ++ showArgs xs

showArgs :: (HasValues t, Show t) => [OpExpr t] -> String
showArgs args = "(" ++ intercalate "," (map show args) ++ ")"
