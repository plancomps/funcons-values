
module Funcons.Operations.Libraries where

import qualified Data.Map as M

import Funcons.Operations.Expr

type Library t = M.Map OP (ValueOp t)

libFromList :: [(OP, ValueOp t)] -> Library t
libFromList = M.fromList

libLookup :: OP -> Library t -> Maybe (ValueOp t)
libLookup = M.lookup

libUnite :: [Library t] -> Library t
libUnite = M.unions

