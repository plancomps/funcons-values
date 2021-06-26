{-# LANGUAGE InstanceSigs #-}

module Funcons.Operations.Expr where

import Funcons.Operations.Values
import Control.Monad (ap)

type OP = String

data Result t = SortErr           String -- sort mismatch
              | DomErr            String -- domain mismatch (in case of partial op)
              | ArityErr          String
              | ProjErr           String -- cannot project to a value
              | Normal            t
              | Nondeterministic  [Result t] 
              deriving Show

choice []  = error "Nondeterministic: no choice given"
choice [t] = t
choice ts  = Nondeterministic ts
 
type NullaryOp t  = Result t
type UnaryOp t    = t -> Result t
type BinaryOp t   = t -> t -> Result t
type TernaryOp t  = t -> t -> t -> Result t
type NaryOp t     = [t] -> Result t

type NullaryVOp t  = Result t
type UnaryVOp t    = Values t -> Result t
type BinaryVOp t   = Values t -> Values t -> Result t
type TernaryVOp t  = Values t -> Values t -> Values t -> Result t
type NaryVOp t     = [Values t] -> Result t

data OpExpr t  
      = ValExpr         (Values t)
      | TermExpr        t  
      | NullaryOp   OP  (NullaryOp t)                                    
      | UnaryOp     OP  (UnaryOp t)     (OpExpr t)                       
      | BinaryOp    OP  (BinaryOp t)    (OpExpr t) (OpExpr t)            
      | TernaryOp   OP  (TernaryOp t)   (OpExpr t) (OpExpr t) (OpExpr t) 
      | NaryOp      OP  (NaryOp t)      [OpExpr t]  
      | InvalidOp   OP  String          [OpExpr t]
      | RewritesTo  OP  (OpExpr t)      [OpExpr t]

vNullaryOp :: OP -> NullaryVOp t -> OpExpr t
vNullaryOp nm op = NullaryOp nm op

vUnaryOp :: HasValues t => OP -> UnaryVOp t -> OpExpr t -> OpExpr t
vUnaryOp nm op = UnaryOp nm op'
  where op' t = case project t of
                  Nothing -> ProjErr nm
                  Just v  -> op v

vBinaryOp :: HasValues t => OP -> BinaryVOp t -> OpExpr t -> OpExpr t -> OpExpr t
vBinaryOp nm op = BinaryOp nm op'
  where op' x y = case (project x, project y) of
                  (Just v1,Just v2) -> op v1 v2 
                  _                 -> ProjErr nm

vTernaryOp :: HasValues t => OP -> TernaryVOp t -> OpExpr t -> OpExpr t -> OpExpr t -> OpExpr t
vTernaryOp nm op = TernaryOp nm op'
  where op' x y z = case (project x, project y, project z) of
                  (Just v1,Just v2, Just v3)  -> op v1 v2 v3
                  _                           -> ProjErr nm

vNaryOp :: HasValues t => OP -> NaryVOp t -> [OpExpr t] -> OpExpr t
vNaryOp nm op = NaryOp nm op'
  where op' ts = case mapM project ts of
                  Just vs -> op vs
                  Nothing -> ProjErr nm

opName :: OpExpr t -> OP
opName (ValExpr _) = error "opName val"
opName (TermExpr _) = error "opName term"
opName (NullaryOp op _) = op
opName (UnaryOp op _ _) = op
opName (BinaryOp op _ _ _) = op
opName (TernaryOp op _ _ _ _) = op
opName (NaryOp op _ _) = op
opName (InvalidOp op _ _) = op
opName (RewritesTo op _ _) = op

data ValueOp t      = NullaryExpr (NullaryExpr t)
                    | UnaryExpr (UnaryExpr t)
                    | BinaryExpr (BinaryExpr t)
                    | TernaryExpr (TernaryExpr t)
                    | NaryExpr (NaryExpr t)

type NullaryExpr t  = OpExpr  t
type UnaryExpr t    = OpExpr t -> OpExpr  t
type BinaryExpr t   = OpExpr t -> OpExpr t -> OpExpr  t
type TernaryExpr t  = OpExpr t -> OpExpr t -> OpExpr t -> OpExpr  t
type NaryExpr t     = [OpExpr t] -> OpExpr t

nullaryOp :: NullaryExpr t ->  [OpExpr t] -> OpExpr t
nullaryOp f [] = f
nullaryOp f xs = arityErr 0 (opName f) xs

unaryOp :: UnaryExpr t ->  [OpExpr t] -> OpExpr t
unaryOp f [x] = f x
unaryOp f xs = arityErr 1 (opName (f undefined)) xs

binaryOp :: BinaryExpr t ->  [OpExpr t] -> OpExpr t
binaryOp f [x,y] = f x y
binaryOp f xs = arityErr 2 (opName (f undefined undefined)) xs

ternaryOp :: TernaryExpr t -> [OpExpr t] -> OpExpr t
ternaryOp f [x,y,z] = f x y z 
ternaryOp f xs = arityErr 3 (opName (f undefined undefined undefined)) xs

arityErr :: Int -> OP -> [OpExpr t] -> OpExpr t
arityErr i op = InvalidOp op ("not applied to " ++ show i ++ " arguments")

applyExpr :: HasValues t => OpExpr t -> Result t
applyExpr expr = case expr of
  ValExpr v                     -> Normal (inject v)
  TermExpr t                    -> Normal t
  InvalidOp _ err _             -> ArityErr err
  NullaryOp _ f                 -> f
  UnaryOp _ f x                 -> f =<< applyExpr x
  BinaryOp _ f x y              -> do   xv <- applyExpr x 
                                        yv <- applyExpr y
                                        f xv yv
  TernaryOp _ f x y z           -> do   xv <- applyExpr x
                                        yv <- applyExpr y
                                        zv <- applyExpr z
                                        f xv yv zv
  NaryOp _ f xs                 -> f =<< mapM applyExpr xs
  RewritesTo _ e1 _             -> applyExpr e1

instance Functor Result where
  fmap f (SortErr err)          = SortErr err
  fmap f (ProjErr err)          = ProjErr err
  fmap f (DomErr err)           = DomErr err
  fmap f (ArityErr err)         = ArityErr err
  fmap f (Normal v)             = Normal (f v)
  fmap f (Nondeterministic vs)  = Nondeterministic (fmap (fmap f) vs)

instance Applicative Result where
  pure = Normal
  (<*>) = ap

instance Monad Result where
  return = Normal
  p >>= q = case p of
    SortErr err           -> SortErr err
    ProjErr err           -> ProjErr err
    DomErr err            -> DomErr err
    ArityErr err          -> ArityErr err
    Normal f              -> q f 
    Nondeterministic fs   -> Nondeterministic (map (>>= q) fs)

-- helper / smart constructors
{-
nullaryOp :: OP -> Result t -> OpExpr t
nullaryOp = NullaryOp

unaryOp :: IsOperand o => OP -> o t -> UnaryOp t -> OpExpr t
unaryOp nm o res = UnaryOp nm (toOp o) res

binaryOp :: (IsOperand o1, IsOperand o2) => OP -> o1 t -> o2 t -> BinaryOp t -> OpExpr t
binaryOp nm x y res = BinaryOp nm (toOp x) (toOp y) res

ternaryOp :: IsOperand o => OP -> o t -> o t -> o t -> TernaryOp t -> OpExpr t
ternaryOp nm x y z op = TernaryOp nm (toOp x) (toOp y) (toOp z) op

naryOp :: IsOperand o => OP -> [o t] -> NaryOp t -> OpExpr t
naryOp nm xs op = NaryOp nm (map toOp xs) op

rewritesTo :: (IsOperand o1, IsOperand o2) => OP -> [o1 t] -> o2 t -> OpExpr t 
rewritesTo nm xs op = RewritesTo nm (map toOp xs) (toOp op)
-}
