{-# LANGUAGE OverloadedStrings #-}

module Funcons.Operations.Maps where

import Funcons.Operations.Booleans
import Funcons.Operations.Internal
import Funcons.Operations.Sets
import qualified Data.Map as M
import qualified Data.Set as S 
import Data.Maybe (fromJust)

library :: (HasValues t, Ord t) => Library t
library = libFromList [
    ("map-empty", NullaryExpr map_empty)
  , ("map-singleton", BinaryExpr map_singleton)
  , ("is-map-empty", UnaryExpr is_map_empty)
  , ("map-insert", TernaryExpr map_insert)
  , ("map-lookup", BinaryExpr map_lookup)
  , ("lookup", BinaryExpr map_lookup)
  , ("map-domain", UnaryExpr domain)
  , ("domain", UnaryExpr domain)
  , ("dom", UnaryExpr domain)
  , ("map-delete", BinaryExpr map_delete)
  , ("is-in-domain", BinaryExpr is_in_domain)
  , ("map-unite", NaryExpr map_unite)
  , ("map-override", NaryExpr map_override_)
  , ("maps", BinaryExpr Funcons.Operations.Maps.maps)
  , ("map", NaryExpr map_)
  , ("map-elements", UnaryExpr map_elements)
  ]

map_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
map_ = vNaryOp "map" op
  where op vs | areBindings, allDistinct = Normal $ inject $ Map $ M.fromListWith const assocs
              | not (areBindings)  = SortErr "map not applied to pairs"
              | otherwise       = Normal $ inject null_value__ 
          where areBindings   = all isBinding vs
                  where isBinding (ADTVal "tuple" (k:vs))
                          | Just _ <- project k
                          , Just _ <- mapM project vs = True
                        isBinding _                   = False
                assocs     = map mkBinding vs
                  where mkBinding (ADTVal "tuple" (k:vs)) = 
                          (fromJust (project k), map (fromJust . project) vs)
                        mkBinding _ = error "assert: map$mkBinding"
                allDistinct = recDistinct (map fst assocs)
                  where recDistinct [] = True
                        recDistinct (k:ks) = not (k `S.member` (S.fromList ks))
                                             && recDistinct ks    
  
maps_ :: HasValues t => [OpExpr t] -> OpExpr t
maps_ = binaryOp Funcons.Operations.Maps.maps
maps :: HasValues t => OpExpr t -> OpExpr t -> OpExpr t
maps = vBinaryOp "maps" op
  where op t1 t2 = Normal $ injectT (Funcons.Operations.Internal.maps (inject t1) (inject t2))

map_empty_ :: HasValues t => [OpExpr t] -> OpExpr t 
map_empty_ = nullaryOp map_empty
map_empty :: HasValues t => OpExpr t 
map_empty = NullaryOp "map-empty" (Normal $ inject (Map M.empty))

map_singleton_ :: (HasValues t,Ord t) => [OpExpr t] -> OpExpr t
map_singleton_ = binaryOp map_singleton
map_singleton :: (HasValues t, Ord t) => OpExpr t -> OpExpr t -> OpExpr t 
map_singleton k v = RewritesTo "map-insert" (map_insert map_empty k v) [k,v]

is_map_empty_ :: HasValues t => [OpExpr t] -> OpExpr t
is_map_empty_ = unaryOp is_map_empty
is_map_empty :: HasValues t => OpExpr t -> OpExpr t 
is_map_empty = vUnaryOp "is-map-empty" op 
  where op (Map m)  = Normal $ inject $ tobool (null m)
        op _        = SortErr "is-map-empty(M) not applied to a map"

map_insert_ :: (HasValues t, Ord t) => [OpExpr t] -> OpExpr t
map_insert_ = ternaryOp map_insert 
map_insert :: (HasValues t, Ord t) => OpExpr t -> OpExpr t -> OpExpr t -> OpExpr t
map_insert = vTernaryOp "map-insert" op
  where op xv k v = case xv of 
              Map m -> Normal $ inject $ Map (M.insert k [v] m)
              _     -> SortErr "map-insert(M,K,V) not applied to a map (first argument)"

map_lookup_ :: (HasValues t, Ord t) => [OpExpr t] -> OpExpr t
map_lookup_ = binaryOp map_lookup
map_lookup :: (HasValues t, Ord t) => OpExpr t -> OpExpr t -> OpExpr t
map_lookup = vBinaryOp "map-lookup" op
  where op xv k = case xv of 
                    Map m -> Normal $ inject $ multi_ $ maybe [] id $ M.lookup k m 
                    _ -> SortErr "map-lookup(M,V) not applied to a map and a value"

map_delete_ :: (HasValues t, Ord t) => [OpExpr t] -> OpExpr t
map_delete_ = binaryOp map_delete
map_delete :: (HasValues t, Ord t) => OpExpr t -> OpExpr t -> OpExpr t
map_delete = vBinaryOp "map-delete" op
  where op (Map m) (Set s) = Normal $ inject $ Map (foldr M.delete m s)
        op _ _ = SortErr "map-delete(M,S) not applied to a map and a set"

is_in_domain_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t 
is_in_domain_ = binaryOp is_in_domain
is_in_domain :: (Ord t, HasValues t) => OpExpr t -> OpExpr t -> OpExpr t 
is_in_domain x y = RewritesTo "is-in-domain" (is_in_set x (domain y)) [x,y] 

domain_ :: (HasValues t, Ord t) => [OpExpr t] -> OpExpr t
domain_ = unaryOp domain
domain :: (HasValues t, Ord t) => OpExpr t -> OpExpr t
domain = vUnaryOp "domain" op
  where op (Map m)  = Normal $ inject $ Set $ S.fromList $ M.keys m
        op _        = SortErr "domain(M) not applied to a map"

map_override_ :: (HasValues t, Ord t) => [OpExpr t] -> OpExpr t
map_override_ = vNaryOp "map-override" op
  where op vs | all isMap vs = Normal $ inject $ Map (M.unions (map toMap vs))
        op _ = SortErr "map-override not applied to maps"

map_unite_ :: (HasValues t, Ord t) => [OpExpr t] -> OpExpr t
map_unite_ = map_unite 
map_unite :: (HasValues t, Ord t) => [OpExpr t] -> OpExpr t
map_unite = vNaryOp "map-unite" op
  where op args 
          | all isMap args =
              let maps = map toMap args
                  domains = map (M.keysSet) maps
              in if all (null . uncurry S.intersection) (allDomainPairs domains)
                  then Normal $ inject $ Map $ M.unions maps
                  else Normal $ inject null__ 
          | otherwise     = SortErr "map-unite(M1,...,Mn) not applied to maps"

toMap (Map m) = m
toMap _       = error "map_unite"

allDomainPairs :: [a] -> [(a,a)] 
allDomainPairs (x:xs) = [ (x,y)  | y <- xs ] ++ allDomainPairs xs
allDomainPairs [] = []

map_elements_ :: (Ord t, HasValues t) => [OpExpr t] -> OpExpr t
map_elements_ = unaryOp map_elements
map_elements :: (Ord t, HasValues t) => OpExpr t -> OpExpr t
map_elements = vUnaryOp "map-elements" op
  where op (Map m) = Normal $ inject $ multi $ map inject $ M.foldrWithKey combine [] m
          where combine k vs ls = ADTVal "tuple" (inject k : map inject vs):ls
        op _ = SortErr "map-elements not applied to a map"
