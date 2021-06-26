
module Funcons.Operations.Patterns where

import Funcons.Operations.Values

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MultiSet as MS

data VPattern t = VPMVar MVar 
                | VPSeqVar MVar SeqSortOp
                | VPAny
                | VPADT Name [VPattern t]
                | VPLit (Values t)
                | VPTuple [VPattern t]
                | VPList [VPattern t]
                | VPThunk (VPattern t)
                deriving (Ord, Eq)

type MetaEnv t = M.Map MVar [t]

envEmpty :: MetaEnv t
envEmpty = M.empty

envUnite :: MetaEnv t -> MetaEnv t -> MetaEnv t
envUnite = flip M.union

bind :: MVar -> t -> MetaEnv t
bind v t = M.singleton v [t]

termBinding :: MetaEnv t -> MVar -> Maybe t 
termBinding m k = case M.lookup k m of
  Just [t] -> Just $ t
  _        -> Nothing 

termsBinding :: MetaEnv t -> MVar -> Maybe [t]
termsBinding m k = case M.lookup k m of
  Just ts  -> Just $ ts
  Nothing  -> Nothing 


valBinding :: HasValues t => MetaEnv t -> MVar -> Maybe (Values t)
valBinding m k = case M.lookup k m of
  Just [t] -> project t
  _        -> Nothing 

valsBinding :: HasValues t => MetaEnv t -> MVar -> Maybe [Values t]
valsBinding m k = case M.lookup k m of
  Just ts  -> mapM project ts
  _        -> Nothing 

match :: (HasValues t, Eq t) => VPattern t -> t -> Maybe (MetaEnv t)
match p t = case project t of
              Just v  -> matchVal p v
              _       -> case p of  
                          VPAny         -> Just envEmpty
                          VPMVar var    -> Just $ M.singleton var [t]
                          VPSeqVar _ _  -> matches [p] [t]
                          _             -> Nothing

matches :: (HasValues t, Eq t) => [VPattern t] -> [t] -> Maybe (MetaEnv t)
matches a b = case (b, a) of
  ([], [])    -> Just envEmpty  -- consumed both terms and patterns
  (_, [])     -> Nothing        -- not all terms have been matched
  (ts,(VPSeqVar mvar op):ps) -> (matchSeq id op mvar <&> matches ps) ts
  ([],_)      -> Nothing
  (t:ts,p:ps) -> do   env1  <- match p t
                      env2  <- matches ps ts 
                      return (env1 `envUnite` env2)

 
matchVal :: (HasValues t, Eq t) => VPattern t -> Values t -> Maybe (MetaEnv t)
matchVal VPAny _ = Just envEmpty 
matchVal (VPMVar var) v = Just $ M.singleton var [inject v]
matchVal (VPSeqVar var op) (Tuple vs) = matchVals [VPSeqVar var op] vs
matchVal (VPSeqVar var op) v = matchVals [VPSeqVar var op] [v]
matchVal (VPLit v1) v2 = if v1 === v2 then Just envEmpty else Nothing
matchVal (VPTuple ps) (Tuple vs) = matchVals ps vs
matchVal (VPTuple _) _ = Nothing
matchVal (VPList ps) (List vs) = matchVals ps vs
matchVal (VPList _) _ = Nothing
matchVal (VPADT cons args) p = case p of 
  ADTVal cons' args' | cons == cons'  -> matchVals args args' 
  _                                   -> Nothing
matchVal (VPThunk p) v = case v of  Thunk t -> match p t
                                    _       -> Nothing

matchVals :: (HasValues t,Eq t) => [VPattern t] -> [Values t] -> Maybe (MetaEnv t)
matchVals a b = case (b, a) of
  ([], [])    -> Just envEmpty  -- consumed both terms and patterns
  (_, [])     -> Nothing        -- not all terms have been matched
  (vs,(VPSeqVar mvar op):ps) -> (matchSeq (map inject) op mvar 
                                  <&> matchVals ps) vs
  ([],_)      -> Nothing
  (v:vs,p:ps) -> do   env1  <- matchVal p v
                      env2  <- matchVals ps vs 
                      return (env1 `envUnite` env2)

matchSeq :: ([a] -> [t]) -> SeqSortOp -> MVar -> [a] -> [(MetaEnv t, [a])]
matchSeq cons op var vs = map bind ks 
  where ks = case op of SeqRange l Nothing  -> [m,m-1..l]
                        SeqRange l (Just u) -> [u,u-1..l]
          where m = length vs
        bind k = (M.singleton var (cons (take k selected)), rest)
          where (selected, rest) = splitAt k vs

(p <&> q) vs = select (p vs)
  where select [] = Nothing
        select ((env1, vs'):xs) = case q vs' of
          Just env2 -> Just (env1 `envUnite` env2)
          Nothing   -> select xs

subsVal :: (Ord t, HasValues t) => Bool -> MetaEnv t -> (t -> t) -> Values t -> Values t 
subsVal bindToThunk gam subsT v = case v of 
  VHolder x | Just v2 <- valBinding gam x -> v2 
            | bindToThunk, Just t2 <- termBinding gam x -> Thunk t2
  ADTVal nm vs  -> ADTVal nm (concatMap (subsVals bindToThunk gam subsT) vs)
  List vs       -> List (concatMap (subsVals bindToThunk gam subsT) vs)
  Map ms        -> Map (M.map (subsVal bindToThunk gam subsT) $ M.mapKeys (subsVal bindToThunk gam subsT) ms)
  Multiset vs   -> Multiset $ MS.fold op MS.empty vs  
    where op v set = MS.fromList (subsVals bindToThunk gam subsT v) `MS.union` set
  Set vs        -> Set $ S.fold op S.empty vs
    where op v set = S.fromList (subsVals bindToThunk gam subsT v) `S.union` set
  Tuple vs      -> Tuple $ concatMap (subsVals bindToThunk gam subsT) vs
  Vector vs     -> Vector $ fmap (subsVal bindToThunk gam subsT) vs
  Thunk  t      -> Thunk (subsT t)
  _             -> v 

subsVals :: (Ord t, HasValues t) => Bool -> MetaEnv t -> (t -> t) -> Values t -> [Values t]
subsVals bindToThunk gam subsT v = case v of
  VHolder x | Just vs <- valsBinding gam x  -> vs
  _                                         -> [subsVal bindToThunk gam subsT v]

