{-# LANGUAGE UndecidableInstances, OverloadedStrings, TupleSections, RankNTypes, FlexibleInstances, LambdaCase #-}

module Funcons.Operations.Values where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.MultiSet as MS

import qualified Data.Char as C
import Data.Text (Text, unpack)
import Data.List (intercalate)
import Data.Maybe (fromJust,isJust)
import Data.String

import Control.Monad (liftM2)
import Control.Arrow ((***))

type Name = Text
type MVar = String

-- | 
-- This datatype provides a number of builtin value types.
-- The type `t` is expected to be a super-type of `Values t`,
-- such that there is a projection and injection between `t` and `Values t`,
-- (see 'HasValues')
data Values t   = ADTVal Name [t]
                | Atom String
                | ComputationType (ComputationTypes t)
                | Float Double 
                | IEEE_Float_32 Float
                | IEEE_Float_64 Double
                | Int Integer
                | Map (ValueMaps (Values t))
                | Multiset (MS.MultiSet (Values t))
                | Nat Integer
                | Rational Rational
                | Set (ValueSets (Values t))
                | Vector (ValueVectors (Values t))
                | VAny -- used whenever funcon terms may have holes in them
                       -- currently only the case in "downwards" flowing signals
                | ValSeq [t] -- represents a multitude of values
        deriving (Eq,Ord,Show,Read)

ascii_cons, unicode_cons :: Name 
ascii_cons = "ascii-character"
unicode_cons = "unicode-character"

tuple :: HasValues t => [Values t] -> Values t
tuple = ADTVal "tuple" . map inject

list :: HasValues t => [Values t] -> Values t
list = ADTVal "list" . map inject

set :: (Ord t, HasValues t) => [Values t] -> Values t
set = Set . S.fromList

vector :: HasValues t => [Values t] -> Values t
vector = ADTVal "vector" . map inject 

multi :: HasValues t => [t] -> Values t 
multi = ValSeq 

multi_ :: HasValues t => [Values t] -> Values t
multi_ = multi . map inject

instance HasValues t => IsString (Values t) where
  fromString = ADTVal "list" . map (inject . mk_unicode_characters)

type ValueMaps t      = M.Map t [t] 
type ValueSets t      = S.Set t
type ValueVectors t   = V.Vector t

-- | Postfix operators for specifying sequences.
data SeqSortOp = StarOp | PlusOp | QuestionMarkOp
                deriving (Show, Eq, Ord, Read)


-- | Computation type /S=>T/ reflects a type of term
-- whose given value is of type /S/ and result is of type /T/.
data ComputationTypes t = Type (Types t) -- | /=>T/
                        | ComputesType (Types t) -- | /S=>T/
                        | ComputesFromType (Types t) (Types t)
                        deriving (Ord,Eq,Show, Read)

-- | Representation of builtin types.
data Types t= ADTs
            | ADT Name [t]
            | AnnotatedType (Types t) SeqSortOp
            | AsciiCharacters
            | ISOLatinCharacters
            | BMPCharacters
            | Atoms
            | IntegersFrom Integer -- value-dependent type
            | IntegersUpTo Integer
            | Characters
            | Complement (Types t)
            | ComputationTypes
            | EmptyType
            | IEEEFloats IEEEFormats
            | Integers
            | Intersection (Types t) (Types t)
            | Naturals
            | NullType 
            | Rationals
            | Types
            | UnicodeCharacters
            | Union (Types t) (Types t)
            | Values
              deriving (Ord,Eq,Show,Read)

sets :: HasValues t => Types t -> Types t
sets t = ADT "sets" [injectT t]

multisets :: HasValues t => Types t -> Types t
multisets t = ADT "multisets" [injectT t]

maps :: HasValues t => t -> t -> Types t
maps k v = ADT "maps" [k, v]

vectors :: HasValues t => Types t -> Types t
vectors t = ADT "vectors" [injectT t]

class HasValues t where
  project :: t -> Maybe (Values t)
  inject  :: Values t -> t
class HasComputationTypes t where
  projectCT :: t -> Maybe (ComputationTypes t)
  injectCT  :: ComputationTypes t -> t
class HasTypes t where
  projectT  :: t -> Maybe (Types t)
  injectT   :: Types t -> t
instance HasValues t => HasComputationTypes t where
  projectCT v = project v >>= \case
                ComputationType t -> Just t
                _      -> Nothing
                
  injectCT  = inject . ComputationType
instance HasComputationTypes t => HasTypes t where
  projectT ct = projectCT ct >>= \case 
                  Type t -> Just t 
                  _      -> Nothing
  injectT = injectCT . Type

data IEEEFormats = Binary32 | Binary64
        deriving (Enum,Show,Eq,Ord,Read)

-- Specialised version of 'fmap'
vmap :: (Ord b) => (a -> b) -> Values a -> Values b
vmap f v = case v of
    ADTVal nm ts      -> ADTVal nm (map f ts)
    Atom a            -> Atom a
    ComputationType t -> ComputationType (fmap f t)
    Float f           -> Float f
    IEEE_Float_32 f   -> IEEE_Float_32 f
    IEEE_Float_64 f   -> IEEE_Float_64 f
    Int i             -> Int i
    Map m             -> Map $ M.fromList $ map (vmap f *** fmap (vmap f)) $ M.assocs m
    Set s             -> Set $ S.map (vmap f) s
    Multiset ms       -> Multiset $ MS.map (vmap f) ms
    Nat n             -> Nat n
    Rational r        -> Rational r
    Vector v          -> Vector $ V.map (vmap f) v
    VAny              -> VAny 
    ValSeq ts         -> ValSeq (map f ts)

 
traverseV :: (Ord b, Monad m, HasValues a, HasValues b) => 
  (a -> m b) -> Values a -> m (Values b)
traverseV f = traverseVM f (mapM f)

traverseVM :: (Ord b, Monad m, HasValues a, HasValues b) => 
  (a -> m b) -> ([a] -> m [b]) -> Values a -> m (Values b)
traverseVM f fs v = case v of
    ADTVal nm vs      -> return . ADTVal nm =<< fs vs
    Atom a            -> return $ Atom a
    ComputationType t -> return . ComputationType   =<< traverseCTM f fs t
    Float f           -> return $ Float f
    IEEE_Float_32 f   -> return $ IEEE_Float_32 f
    IEEE_Float_64 f   -> return $ IEEE_Float_64 f
    Int i             -> return $ Int i
    Map m             -> do 
        let (keys, valss) = unzip (M.assocs m)
        keys' <- map (fromJust . project) <$> fs (map inject keys)
        vals' <- map (map (fromJust . project)) <$> mapM (fs . map inject) valss
        return (Map $ M.fromList $ zip keys' vals')
    Set s             -> return . Set . S.fromList . map (fromJust . project) =<< fs (map inject $ S.toList s)
    Multiset ms       -> return . Multiset . MS.fromList . map (fromJust . project) =<< fs (map inject $ MS.toList ms)
    Nat n             -> return $ Nat n
    Rational r        -> return $ Rational r
    Vector v          -> return . Vector . V.fromList . map (fromJust . project) =<< fs (map inject $ V.toList v)
    VAny -> return VAny
    ValSeq ts -> ValSeq <$> fs ts

traverseT :: (Ord b, Monad m, HasValues a, HasValues b) => 
  (a -> m b) -> Types a -> m (Types b)
traverseT f = traverseTM f (mapM f)
traverseTM :: (Ord b, Monad m, HasValues a, HasValues b) => 
  (a -> m b) -> ([a] -> m [b]) -> Types a -> m (Types b)
traverseTM f fs t = case t of
  ADTs -> return ADTs
  ADT nm ts -> return . ADT nm =<< fs ts
  AsciiCharacters -> return AsciiCharacters
  ISOLatinCharacters -> return ISOLatinCharacters
  BMPCharacters -> return BMPCharacters
  Atoms ->  return Atoms
  AnnotatedType ty op -> AnnotatedType <$> traverseTM f fs ty <*> return op
  Characters -> return Characters
  ComputationTypes -> return ComputationTypes
  Complement t -> Complement <$> traverseTM f fs t  
  IntegersFrom f -> return (IntegersFrom f)
  IntegersUpTo f -> return (IntegersUpTo f)
  Intersection t1 t2 -> Intersection <$> traverseTM f fs t1 <*> traverseTM f fs t2
  NullType -> return NullType
  EmptyType -> return EmptyType
  IEEEFloats i -> return (IEEEFloats i)
  Integers -> return Integers
  Naturals -> return Naturals
  Rationals -> return Rationals
  Types -> return Types 
  UnicodeCharacters -> return UnicodeCharacters
  Union t1 t2 -> Union <$> traverseTM f fs t1 <*> traverseTM f fs t2
  Values -> return Values

traverseCTM f fs t = case t of
  Type t -> Type <$> traverseTM f fs t
  ComputesType t -> ComputesType <$> traverseTM f fs t
  ComputesFromType ty ty2 -> ComputesFromType <$> traverseTM f fs ty
                                              <*> traverseTM f fs ty2

structVcompare :: (Monoid m, HasValues a, HasValues b) => 
  (a -> b -> Maybe m) -> Values a -> Values b -> Maybe (Maybe m)
structVcompare comp = structVMcompare comp comps
  where comps xs ys | length xs == length ys = fmap mconcat $ sequence $ zipWith comp xs ys
                    | otherwise = Nothing

structCTMcompare :: (Monoid m, HasValues a, HasValues b) => 
  (a -> b -> Maybe m) -> ([a] -> [b] -> Maybe m) ->
    ComputationTypes a -> ComputationTypes b -> (Maybe (Maybe m))
structCTMcompare comp comps va vb = case (va,vb) of
  (Type x, Type y)  -> structTMcompare comp comps x y 
  (Type _,_)        -> Nothing
  (_, Type _)       -> Nothing
  (ComputesType x, ComputesType y)  -> structTMcompare comp comps x y 
  (ComputesType _, _)               -> Nothing 
  (_, ComputesType _)               -> Nothing
  (ComputesFromType x y, ComputesFromType x' y') -> 
    liftM2 mappend (structTMcompare comp comps x x') (structTMcompare comp comps y y')

structVMcompare :: (Monoid m, HasValues b, HasValues a) => 
  (a -> b -> Maybe m) -> ([a] -> [b] -> Maybe m) -> 
    Values a -> Values b -> Maybe (Maybe m)
structVMcompare comp comps va vb = case (va, vb) of
  (ADTVal nm1 vs1, ADTVal nm2 vs2) 
    | nm1 == nm2 -> Just $ comps vs1 vs2
  (ADTVal _ _, _) -> Nothing
  (_, ADTVal _ _) -> Nothing
  (Atom x, Atom y) | x == y   -> Just (Just mempty)
  (Atom _, _)                 -> Nothing
  (_, Atom _)                 -> Nothing
  (ComputationType x
    ,ComputationType y)       -> structCTMcompare comp comps x y 
  (_, ComputationType x)      -> Nothing
  (ComputationType _, _)      -> Nothing
  (Float x, Float y) | x == y -> Just (Just mempty)
  (Float _, _)                -> Nothing
  (_, Float _)                -> Nothing
  (IEEE_Float_32 x, IEEE_Float_32 y) | x == y -> Nothing
  (IEEE_Float_32 _, _)                        -> Nothing
  (_, IEEE_Float_32 _)                        -> Nothing
  (IEEE_Float_64 x, IEEE_Float_64 y) | x == y -> Nothing
  (IEEE_Float_64 _, _)                        -> Nothing
  (_, IEEE_Float_64 _)                        -> Nothing
  (Int x, Int y) | x == y                     -> Just (Just mempty)
  (Int _, _)                                  -> Nothing
  (_, Int _)                                  -> Nothing
  (Map m1, Map m2) -> Just $ liftM2 mappend (comps' (M.keys m1) (M.keys m2))
                                            (comps' (map list $ M.elems m1)
                                                    (map list $ M.elems m2))
  (Map _, _)      -> Nothing
  (_, Map _)      -> Nothing
  (Set x, Set y) -> Just $ comps' (S.toList x) (S.toList y)
  (Set _, _)      -> Nothing
  (_, Set _)      -> Nothing
  (Multiset x, Multiset y) -> Just $ comps (map inject $ MS.toList x) 
                                           (map inject $ MS.toList y)
  (Multiset _, _)           -> Nothing
  (_, Multiset _)           -> Nothing
  (Nat x, Nat y)  | x == y  -> Just (Just mempty)
  (Nat _, _)                -> Nothing
  (_, Nat _)                -> Nothing
  (Rational x, Rational y) | x == y -> Just (Just mempty)
  (Rational _, _)                   -> Nothing
  (_, Rational _)                   -> Nothing
  (Vector x, Vector y) -> Just $ comps (map inject $ V.toList x) (map inject $ V.toList y)
  (VAny, VAny)  -> Just (Just mempty)
  (_, VAny)     -> Nothing
  (VAny, _)     -> Nothing
  (ValSeq ts, ValSeq ts') -> Just (comps ts ts')
  (ValSeq _, _)           -> Nothing
  (_, ValSeq _)           -> Nothing
  where comps' xs ys = comps (map inject xs) (map inject ys)

structTMcompare :: (Monoid m, HasValues a, HasValues b) => 
  (a -> b -> Maybe m) -> ([a] -> [b] -> Maybe m) -> 
    Types a -> Types b -> Maybe (Maybe m)
structTMcompare comp comps ta tb = case (ta, tb) of
  (ADTs, ADTs)                        -> Just (Just mempty)
  (ADTs, _)                           -> Nothing
  (_, ADTs)                           -> Nothing
  (ADT nm1 ts, ADT nm2 ts') | nm1 == nm2 -> Just $ comps ts ts'
  (ADT _ _, _)                        -> Nothing
  (_, ADT _ _)                        -> Nothing
  (Atoms, Atoms)                      -> Just (Just mempty)
  (Atoms, _)                          -> Nothing
  (_, Atoms)                          -> Nothing
  (AsciiCharacters, AsciiCharacters)  -> Just (Just mempty) 
  (AsciiCharacters, _)                -> Nothing
  (_, AsciiCharacters)                -> Nothing
  (ISOLatinCharacters, ISOLatinCharacters)  -> Just (Just mempty) 
  (ISOLatinCharacters, _)                -> Nothing
  (_, ISOLatinCharacters)                -> Nothing
  (BMPCharacters, BMPCharacters)  -> Just (Just mempty) 
  (BMPCharacters, _)                -> Nothing
  (_, BMPCharacters)                -> Nothing
  (AnnotatedType t1 op1, AnnotatedType t2 op2) | op1 == op2 -> structTMcompare comp comps t1 t2
  (AnnotatedType _ _, _)              -> Nothing
  (_, AnnotatedType _ _)              -> Nothing
  (Characters, Characters)            -> Just (Just mempty)
  (Characters, _)                     -> Nothing
  (_, Characters)                     -> Nothing
  (Complement x, Complement y)        -> structTMcompare comp comps x y
  (_, Complement _)                   -> Nothing
  (Complement _, _)                   -> Nothing
  (ComputationTypes, ComputationTypes)-> Just (Just mempty)
  (_, ComputationTypes)               -> Nothing
  (ComputationTypes, _)               -> Nothing
  (IntegersFrom mx, IntegersFrom mx') | mx == mx' -> Just (Just mempty)
  (IntegersFrom _, _)                 -> Nothing
  (_, IntegersFrom _)                 -> Nothing
  (IntegersUpTo mx, IntegersUpTo mx') | mx == mx' -> Just (Just mempty)
  (IntegersUpTo _, _)                   -> Nothing
  (_, IntegersUpTo _)                   -> Nothing
  (EmptyType, EmptyType)              -> Just (Just mempty)
  (_, EmptyType)                      -> Nothing
  (EmptyType, _)                      -> Nothing
  (IEEEFloats x, IEEEFloats y) | x ==y-> Just (Just mempty)
  (IEEEFloats _, _)                   -> Nothing
  (_, IEEEFloats _)                   -> Nothing
  (Integers, Integers)                -> Just (Just mempty)
  (Integers, _)                       -> Nothing
  (_, Integers)                       -> Nothing
  (Intersection x y, Intersection x' y') -> liftM2 mappend (structTMcompare comp comps x x') (structTMcompare comp comps y y')
  (Intersection _ _, _)               -> Nothing
  (_, Intersection _ _)               -> Nothing
  (Naturals, Naturals)                -> Just (Just mempty)
  (_, Naturals)                       -> Nothing
  (Naturals, _)                       -> Nothing
  (NullType, NullType)                -> Just (Just mempty)
  (_, NullType)                       -> Nothing
  (NullType,_)                        -> Nothing
  (Rationals, Rationals)              -> Just (Just mempty)
  (Rationals, _)                      -> Nothing
  (_, Rationals)                      -> Nothing
  (Types, Types)                      -> Just (Just mempty)
  (_, Types)                          -> Nothing
  (Types, _)                          -> Nothing
  (UnicodeCharacters, UnicodeCharacters)  -> Just (Just mempty)
  (UnicodeCharacters, _)                  -> Nothing
  (_, UnicodeCharacters)                  -> Nothing
  (Union u v, Union x y)                  -> liftM2 mappend (structTMcompare comp comps u x) (structTMcompare comp comps v y)
  (Union _ _, _)                          -> Nothing
  (_, Union _ _)                          -> Nothing
  (Values, Values)                        -> Just (Just mempty)

instance Functor Types where
  fmap f t = case t of 
    ADT nm ts           -> ADT nm (map f ts) 
    ADTs                -> ADTs
    AsciiCharacters     -> AsciiCharacters
    ISOLatinCharacters  -> ISOLatinCharacters
    BMPCharacters       -> BMPCharacters
    Atoms               -> Atoms
    AnnotatedType ty op -> AnnotatedType (fmap f ty) op
    Complement t1       -> Complement (fmap f t1)
    ComputationTypes    -> ComputationTypes
    IntegersFrom p      -> IntegersFrom p
    IntegersUpTo p      -> IntegersUpTo p
    Characters          -> Characters   
    EmptyType           -> EmptyType
    IEEEFloats b        -> IEEEFloats b
    Integers            -> Integers
    Intersection t1 t2  -> Intersection (fmap f t1) (fmap f t2)
    Naturals            -> Naturals
    NullType            -> NullType
    Rationals           -> Rationals
    Types               -> Types
    UnicodeCharacters   -> UnicodeCharacters
    Union t1 t2         -> Union (fmap f t1) (fmap f t2)
    Values              -> Values 

instance Functor ComputationTypes where
  fmap f t = case t of
    Type t -> Type $ fmap f t
    ComputesType t -> ComputesType $ fmap f t
    ComputesFromType t1 t2 -> ComputesFromType (fmap f t1) (fmap f t2)

instance Foldable Types where
  foldMap f fa = case fa of 
    ADT _ ts            -> foldMap f ts
    ADTs                -> mempty
    AsciiCharacters     -> mempty
    ISOLatinCharacters  -> mempty
    BMPCharacters       -> mempty
    Atoms               -> mempty
    AnnotatedType ty op -> foldMap f ty
    Characters          -> mempty
    Complement t1       -> foldMap f t1
    ComputationTypes    -> mempty
    IntegersUpTo q      -> mempty
    IntegersFrom q      -> mempty
    Intersection t1 t2  -> foldMap f t1 `mappend` foldMap f t2
    EmptyType           -> mempty
    IEEEFloats b        -> mempty 
    Integers            -> mempty
    Naturals            -> mempty 
    NullType            -> mempty
    Rationals           -> mempty
    Types               -> mempty 
    UnicodeCharacters   -> mempty 
    Union t1 t2         -> foldMap f t1 `mappend` foldMap f t2
    Values              -> mempty 

instance Traversable Types where
  traverse f ta = case ta of 
    ADTs                -> pure ADTs
    ADT nm ts           -> ADT nm <$> traverse f ts
    AsciiCharacters     -> pure AsciiCharacters
    ISOLatinCharacters  -> pure ISOLatinCharacters
    BMPCharacters       -> pure BMPCharacters
    AnnotatedType ty op -> AnnotatedType <$> traverse f ty <*> pure op
    Atoms               -> pure Atoms
    Characters          -> pure Characters
    Complement t        -> Complement <$> traverse f t
    ComputationTypes    -> pure ComputationTypes
    IntegersFrom n      -> pure $ IntegersFrom n
    IntegersUpTo n        -> pure $ IntegersUpTo n
    EmptyType           -> pure EmptyType
    IEEEFloats b        -> pure $ IEEEFloats b
    Integers            -> pure Integers
    Intersection t1 t2  -> Intersection <$> traverse f t1 <*> traverse f t2
    Naturals            -> pure Naturals
    NullType            -> pure NullType
    Rationals           -> pure Rationals
    Types               -> pure Types
    UnicodeCharacters   -> pure UnicodeCharacters
    Union t1 t2         -> Union <$> traverse f t1 <*> traverse f t2
    Values              -> pure Values 

downcastValueType :: Values t -> Types t
downcastValueType (ComputationType (Type t)) = t
downcastValueType (ComputationType (ComputesType t)) = t
downcastValueType (ComputationType (ComputesFromType _ t)) = t
downcastValueType _ = error "valueType: not a type"

-- | Returns the /rational/ representation of a value if it is a subtype.
-- Otherwise it returns the original value.
upcastRationals :: Values t -> Values t
upcastRationals (Nat n) = Rational (toRational n)
upcastRationals (Int i) = Rational (toRational i)
upcastRationals v       = v

-- | Returns the /integer/ representation of a value if it is a subtype.
-- Otherwise it returns the original value.
upcastIntegers :: Values t -> Values t
upcastIntegers (Nat n)  = Int n
upcastIntegers v        = v

-- | Returns the /natural/ representation of a value if it is a subtype.
-- Otherwise it returns the original value.
upcastNaturals :: Values t -> Values t
upcastNaturals (Int i) | i >= 0 = Nat i
upcastNaturals v = v

upcastCharacter :: HasValues t => Values t -> Maybe Char
upcastCharacter (ADTVal c [v]) 
  | Just (Int p) <- project v = Just (C.chr (fromInteger p)) 
upcastCharacter v = Nothing

castType :: HasValues t => Values t -> Maybe (Types t)
castType (ComputationType (Type ty)) = Just ty
castType (ComputationType (ComputesType ty)) = Just ty
castType (ComputationType (ComputesFromType _ ty)) = Just ty
castType _            = Nothing

-- numbers
mk_integers :: Integer -> Values t
mk_integers i   | i >= 0    = mk_naturals i
                | otherwise = Int i

mk_naturals :: Integer -> Values t
mk_naturals = Nat

mk_unicode_characters :: HasValues t => Char -> Values t
mk_unicode_characters = downcast_unicode_characters

-- | 
-- Checks whetDoes not check whether the `unicode-point` of 
downcast_unicode_characters :: HasValues t => Char -> Values t
downcast_unicode_characters c = 
  ADTVal unicode_cons [inject (Int (toInteger (C.ord c)))] 

--- Value specific

(===) :: (HasValues t, Eq t {- UNNECESSARY CONSTRAINT -}) => Values t -> Values t -> Bool
v1 === v2 = isGround v1 && isGround v2 && (v1 == v2)

(=/=) :: (HasValues t, Eq t {- UNNECESSARY CONSTRAINT -}) => Values t -> Values t -> Bool
v1 =/= v2 = isGround v1 && isGround v2 && (v1 /= v2)

isGround :: HasValues t => Values t -> Bool
isGround (ADTVal _ mv)            = all (maybe False isGround . project) mv
isGround (Atom _)                 = True
isGround (Float _)                = True
isGround (IEEE_Float_32 _)        = True
isGround (IEEE_Float_64 _)        = True
isGround (Int _)                  = True
isGround (Map m)                  = all (all isGround) (M.elems m)
isGround (Multiset ms)            = all isGround (MS.elems ms)
isGround (Nat _)                  = True
isGround (ComputationType _)      = True
isGround (Rational _)             = True
isGround (Set s)                  = all isGround (S.toList s)
isGround (Vector v)               = all isGround (V.toList v)
isGround VAny                     = False
isGround (ValSeq ts)              = all (maybe False isGround . project) ts

-- functions that check simple properties of funcons
-- TODO: Some of these are used, and all are exported by Funcons.EDSL
--       But are all of them still needed.  E.g isId doesn't seem very useful now that ids are just strings.
isNat ((Int _))                     = True
isNat _                             = False
isInt ((Int _))                     = True
isInt _                             = False
isEnv f                             = isMap f
isMap ((Map _))                     = True
isMap _                             = False
isSet ((Set _))                     = True
isSet _                             = False
isString_ :: HasValues t => Values t -> Bool
isString_ (ADTVal "list" vs)        = not (null vs) && all (maybe False (isJust . upcastCharacter)) (map project vs)
isString_ _                         = False
isType (ComputationType _)          = True
isType _                            = False
isVec ((Vector _))                  = True
isVec _                             = False

unString :: HasValues t => Values t -> String
unString (ADTVal "list" vs) 
  | Just vs' <- sequence (map (fmap upcastCharacter . project) vs)
  , all isJust vs' = map (\(Just c) -> c) vs'
unString _ = error "unString"

null__ :: Values t
null__ = ADTVal "null" []

null_value__ :: Values t
null_value__ = ADTVal "null-value" []

isNull :: Values t -> Bool
isNull (ADTVal "null" _) = True
isNull (ADTVal "null-value" _) = True
isNull _ = False

isDefinedVal :: Values t -> Bool
isDefinedVal f = not (isNull f)

set_ :: Ord t => [Values t] -> Values t
set_ = Set . S.fromList 

ppValues :: HasValues t => (t -> String) -> Values t -> String
ppValues showT v@(ADTVal "list" vs)
  | isString_ v, not (null vs) = show (unString v)
  | otherwise                  = "[" ++ showArgs_ (map showT vs) ++ "]"
ppValues showT (ADTVal c []) = unpack c
ppValues showT (ADTVal c vs) = unpack c ++ showArgs (map showT vs)
ppValues showT (Atom c)       = "atom("++ c ++")"
ppValues showT (Float f)      = show f
-- rationals
ppValues showT (IEEE_Float_32 f) = show f
ppValues showT (IEEE_Float_64 d) = show d
ppValues showT (Rational r)   = show r
ppValues showT (Int f)        = show f
ppValues showT (Nat f)        = show f
ppValues showT (Map m)        = if M.null m then "map-empty"
                               else "{" ++ key_values ++ "}"
 where key_values = intercalate ", " (map showKP $ M.assocs m)
        where showKP (k,vs) = ppValues showT k ++ " |-> " ++   
                case vs of [v] -> ppValues showT v
                           _   -> showArgs (map (ppValues showT) vs)
ppValues showT (Multiset s) = "{" ++ showArgs (map (ppValues showT) (MS.toList s)) ++ "}"
ppValues showT (Set s) =  "{" ++ showArgs (map (ppValues showT) (S.toList s)) ++ "}"
ppValues showT (Vector v) =  "vector" ++ showArgs (map (ppValues showT) (V.toList v))
ppValues showT (ComputationType ty) = ppComputationTypes showT ty
ppValues showT VAny = "_"
ppValues showT (ValSeq ts) = showArgs_ (map showT ts)

ppComputationTypes :: HasValues t => (t -> String) -> ComputationTypes t -> String
ppComputationTypes showT (Type t) = ppTypes showT t
ppComputationTypes showT (ComputesType ty) = "=>" ++ ppTypes showT ty
ppComputationTypes showT (ComputesFromType s t) = ppTypes showT s ++ "=>" ++ ppTypes showT t

ppTypes :: HasValues t => (t -> String) -> Types t -> String
ppTypes showT (AnnotatedType ty op)  = ppTypes showT ty ++ ppOp op
ppTypes showT (Complement ty)        = "~(" ++ ppTypes showT ty ++ ")"
ppTypes showT ComputationTypes       = "computation-types"
ppTypes showT NullType               = "null-type"
ppTypes showT Atoms                  = "atoms"
ppTypes showT AsciiCharacters        = "ascii-characters"
ppTypes showT ISOLatinCharacters     = "iso-latin-1-characters"
ppTypes showT BMPCharacters          = "basic-multilingual-plane-characters"
ppTypes showT Characters             = "characters"
ppTypes showT (Intersection t1 t2)   = "(" ++ ppTypes showT t1 ++ "&" ++ ppTypes showT t2 ++")"
ppTypes showT (IntegersFrom n)       = "integers-from(" ++ show n ++ ")"
ppTypes showT (IntegersUpTo n)       = "integers-to(" ++ show n ++ ")"
ppTypes showT EmptyType              = "empty-type"
ppTypes showT (UnicodeCharacters)    = "unicode-characters"
ppTypes showT (Integers)             = "integers"
ppTypes showT (Values)               = "values"
ppTypes showT Types                  = "types"
ppTypes showT ADTs                   = "algebraic-datatypes"
ppTypes showT (ADT nm ts)            = unpack nm ++ showArgs (map showT ts)
ppTypes showT (IEEEFloats format)    = "ieee-floats(" ++ show format ++ ")"
ppTypes showT Naturals               = "naturals"
ppTypes showT Rationals              = "rationals"
ppTypes showT (Union ty1 ty2)        = "(" ++ ppTypes showT ty1 ++ "|" ++ ppTypes showT ty2 ++")"

ppOp :: SeqSortOp -> String
ppOp StarOp = "*"
ppOp PlusOp = "+"
ppOp QuestionMarkOp = "?"

showArgs :: [String] -> String
showArgs args = "(" ++ showArgs_ args ++ ")"
showArgs_ :: [String] -> String
showArgs_ args = intercalate "," args 

