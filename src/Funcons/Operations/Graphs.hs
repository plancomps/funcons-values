module Funcons.Operations.Graphs where

import Funcons.Operations.Internal
import Funcons.Operations.Booleans (tobool)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ((\\))

library :: (HasValues t, Ord t) => Library t
library = libFromList [
    ("is-cyclic", UnaryExpr is_cyclic)
  , ("topological-sort", UnaryExpr topological_sort)
  ]

is_cyclic_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
is_cyclic_ = unaryOp is_cyclic
is_cyclic :: (Ord t, HasValues t) => OpExpr t -> OpExpr t
is_cyclic = vUnaryOp "is-cyclic" op
  where op mm | Just m <- toGraph mm = Normal $ inject $ tobool (cyclic m)
        op _ = SortErr "is-cyclic not applied to a graph" 

topological_sort_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
topological_sort_ = unaryOp topological_sort
topological_sort :: (Ord t, HasValues t) => OpExpr t -> OpExpr t
topological_sort = vUnaryOp "topological-sort" op
  where op mm | Just m <- toGraph mm = Normal $ inject $ multi $ map inject $ fst (schedule m)
        op _ = SortErr "topological-sort not applied to a graph"

toGraph :: (Ord t) => Values t -> Maybe (Graph (Values t))
toGraph (Map m) = M.foldrWithKey combine (Just M.empty) m
  where combine k [Set s] mm = fmap (M.insert k s) mm
        combine _ _ _ = Nothing
toGraph _ = Nothing 

-- small graph library
type Graph e = M.Map e (S.Set e)

-- | Get the entry points of the graph
entries :: Eq e => Graph e -> [e]
entries m = M.keys m \\ withIncoming
  where withIncoming = concatMap S.toList (M.elems m)

-- | Delete a node from the graph
delete :: Ord e => e -> Graph e -> Graph e
delete n m = M.map (S.delete n) $ M.delete n m 

-- | Return all nodes in the graph such that if `a -> b` in the graph
-- then `a` occurs before `b` in the result
-- Also returns a graph which, if cyclic, contains all the cycles in the 
-- original graph, corresponding to nodes not in the schedule.
schedule :: (Ord e) => Graph e -> ([e], Graph e)
schedule gr = schedule' gr (entries gr) []
  where schedule' gr []     uset = (uset, gr)
        schedule' gr (e:es) uset = schedule' gr' (entries gr') uset' 
          where uset'       = uset ++ [e]
                gr'         = delete e gr

-- | Checks whether the given grammar contains cycles
cyclic :: (Ord e) => Graph e -> Bool
cyclic gr = not (is_empty (snd (schedule gr)))

-- | Checks whether the given graph is empty
is_empty gr = M.null gr
