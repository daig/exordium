{-# language CPP #-}
{-# language MagicHash #-}
module X.Data.Struct.Set (Set(..), module X.Data.Struct.Set, module X) where
import X.Ops.Fun
import X.Type.Int.I as X
import Data.Bool (Bool(..))
import X.Data.Bool as X
import X.Data.Maybe as X
import X.Stock.Ord
import X.Cast.Coerce.Unsafe
import Prelude (otherwise)
import X.Prim.Any (reallyUnsafePtrEquality#)
import GHC.Magic (lazy) -- TOOD refactor this somewhere in X.Prim
import GHC.Prim (seq) -- TOOD refactor this somewhere in X.Prim
import X.Stock.Eq
import X.Num.Eq
import X.Data.Pair
import X.Data.E as X
import qualified X.Stock.Foldable  as Foldable
import X.Num.Add
import X.Num.Mul
import X.Functor.Fold
import X.Num.Negate
import GHC.Exts (build, lazy) -- TODO: find new homes for this import

ptrEq :: a -> a -> Bool
infix 4 `ptrEq`
{-# inline ptrEq #-}
-- | Checks if two pointers are equal. Yes means yes;
-- no means maybe. The values should be forced to at least
-- WHNF before comparison to get moderately reliable results.
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)

hetPtrEq :: a -> b -> Bool
infix 4 `hetPtrEq`
{-# inline hetPtrEq #-}
-- | Checks if two pointers are equal, without requiring
-- them to have the same type. The values should be forced
-- to at least WHNF before comparison to get moderately
-- reliable results.
hetPtrEq x y = isTrue# (coerce# reallyUnsafePtrEquality# x y)

type Size = Int
data Set a = Bin {-# unpack #-} Size a (Set a) (Set a) | Tip
type role Set nominal

null' :: Set a -> Bool
{-# inline null' #-}
-- | /O(1)/
null' = \case {Tip -> T; Bin{} -> F}

instance Len Set where
  {-# inline len #-}
  len = \case {Tip -> 0; Bin sz _ _ _ -> sz} -- | /O(1)/

elem' :: Ord# a => a -> Set a -> Bool
{-# inlinable elem' #-}
-- | /O(log n)/
elem' = go where
  go !_ Tip = F
  go x (Bin _ y l r) = case compare# x y of LT -> go x l
                                            GT -> go x r
                                            EQ -> T

notElem' :: Ord# a => a -> Set a -> Bool
{-# inlinable notElem' #-}
-- | /O(log n)/
notElem' a t = not (elem' a t)
lookupLT :: Ord# a => a -> Set a -> Maybe a
{-# inlinable lookupLT #-}
-- | /O(log n)/. Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList [3, 5]) `eq#` Nothing
-- > lookupLT 5 (fromList [3, 5]) `eq#` Just 3
lookupLT = goNothing where
    goNothing !_ Tip = Nothing
    goNothing x (Bin _ y l r) | le# x y = goNothing x l
                              | otherwise = goJust x y r
    goJust !_ best Tip = Just best
    goJust x best (Bin _ y l r) | le# x y = goJust x best l
                                | otherwise = goJust x y r

lookupGT :: Ord# a => a -> Set a -> Maybe a
{-# inlinable lookupGT #-}
-- | /O(log n)/. Find smallest element greater than the given one.
--
-- > lookupGT 4 (fromList [3, 5]) `eq#` Just 5
-- > lookupGT 5 (fromList [3, 5]) `eq#` Nothing
lookupGT = goNothing where
    goNothing !_ Tip = Nothing
    goNothing x (Bin _ y l r) | lt# x y = goJust x y l
                              | otherwise = goNothing x r
    goJust !_ best Tip = Just best
    goJust x best (Bin _ y l r) | lt# x y = goJust x y l
                                | otherwise = goJust x best r

lookupLE :: Ord# a => a -> Set a -> Maybe a
{-# inlinable lookupLE #-}
-- | /O(log n)/. Find largest element smaller or equal to the given one.
--
-- > lookupLE 2 (fromList [3, 5]) `eq#` Nothing
-- > lookupLE 4 (fromList [3, 5]) `eq#` Just 3
-- > lookupLE 5 (fromList [3, 5]) `eq#` Just 5
lookupLE = goNothing where
    goNothing !_ Tip = Nothing
    goNothing x (Bin _ y l r) = case compare# x y of LT -> goNothing x l
                                                     EQ -> Just y
                                                     GT -> goJust x y r

    goJust !_ best Tip = Just best
    goJust x best (Bin _ y l r) = case compare# x y of LT -> goJust x best l
                                                       EQ -> Just y
                                                       GT -> goJust x y r

lookupGE :: Ord# a => a -> Set a -> Maybe a
{-# inlinable lookupGE #-}
-- | /O(log n)/. Find smallest element greater or equal to the given one.
--
-- > lookupGE 3 (fromList [3, 5]) `eq#` Just 3
-- > lookupGE 4 (fromList [3, 5]) `eq#` Just 5
-- > lookupGE 6 (fromList [3, 5]) `eq#` Nothing
lookupGE = goNothing where
    goNothing !_ Tip = Nothing
    goNothing x (Bin _ y l r) = case compare# x y of LT -> goJust x y l
                                                     EQ -> Just y
                                                     GT -> goNothing x r

    goJust !_ best Tip = Just best
    goJust x best (Bin _ y l r) = case compare# x y of LT -> goJust x y l
                                                       EQ -> Just y
                                                       GT -> goJust x best r

singleton :: a -> Set a
{-# inline singleton #-}
-- | /O(1)/. Create a singleton set.
singleton x = Bin 1 x Tip Tip

 
{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.

-- See Note: Type of local 'go' function
-- See Note: Avoiding worker/wrapper (in Data.Map.Internal)
insert :: Ord# a => a -> Set a -> Set a
{-# inlinable insert #-}
insert x0 = go x0 x0 where
  go :: Ord# a => a -> a -> Set a -> Set a
  go orig !_ Tip = singleton (lazy orig)
  go orig !x t@(Bin sz y l r) = case compare# x y of
      LT | l' `ptrEq` l -> t
         | otherwise -> balanceL y l' r
         where !l' = go orig x l
      GT | r' `ptrEq` r -> t
         | otherwise -> balanceR y l r'
         where !r' = go orig x r
      EQ | lazy orig `seq` (orig `ptrEq` y) -> t
         | otherwise -> Bin sz (lazy orig) l r


-- Insert an element to the set only if it is not in the set.
-- Used by `union`.

-- See Note: Type of local 'go' function
-- See Note: Avoiding worker/wrapper (in Data.Map.Internal)
insertR :: Ord# a => a -> Set a -> Set a
{-# INLINABLE insertR #-}
insertR x0 = go x0 x0
  where
    go :: Ord# a => a -> a -> Set a -> Set a
    go orig !_ Tip = singleton (lazy orig)
    go orig !x t@(Bin _ y l r) = case compare# x y of
        LT | l' `ptrEq` l -> t
           | otherwise -> balanceL y l' r
           where !l' = go orig x l
        GT | r' `ptrEq` r -> t
           | otherwise -> balanceR y l r'
           where !r' = go orig x r
        EQ -> t

delete :: Ord# a => a -> Set a -> Set a
{-# INLINABLE delete #-}
-- | /O(log n)/. Delete an element from a set.

-- See Note: Type of local 'go' function
delete = go
  where
    go :: Ord# a => a -> Set a -> Set a
    go !_ Tip = Tip
    go x t@(Bin _ y l r) = case compare# x y of
        LT | l' `ptrEq` l -> t
           | otherwise -> balanceR y l' r
           where !l' = go x l
        GT | r' `ptrEq` r -> t
           | otherwise -> balanceL y l r'
           where !r' = go x r
        EQ -> glue l r

{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
isProperSubsetOf :: Ord# a => Set a -> Set a -> Bool
{-# INLINABLE isProperSubsetOf #-}
-- | /O(n`add`m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf s1 s2
    = (len s1 `lt#` len s2) `and` (isSubsetOf s1 s2)


isSubsetOf :: Ord# a => Set a -> Set a -> Bool
{-# INLINABLE isSubsetOf #-}
-- | /O(n`add`m)/. Is this a subset?
-- @(s1 \`isSubsetOf\` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf t1 t2
  = (len t1 `le#` len t2) `and` (isSubsetOfX t1 t2)

isSubsetOfX :: Ord# a => Set a -> Set a -> Bool
{-# INLINABLE isSubsetOfX #-}
isSubsetOfX Tip _ = True
isSubsetOfX _ Tip = False
isSubsetOfX (Bin _ x l r) t
  = found `and` isSubsetOfX l lt `and` isSubsetOfX r gt
  where
    (lt,found,gt) = splitMember x t

{--------------------------------------------------------------------
  Disjoint
--------------------------------------------------------------------}
-- | /O(n`add`m)/. Check whether two sets are disjoint (i.e. their intersection
--   is empty).
--
-- > disjoint (fromList [2,4,6])   (fromList [1,3])     `eq#` True
-- > disjoint (fromList [2,4,6,8]) (fromList [2,3,5,7]) `eq#` False
-- > disjoint (fromList [1,2])     (fromList [1,2,3,4]) `eq#` False
-- > disjoint (fromList [])        (fromList [])        `eq#` True
--
-- @since 0.5.11

disjoint :: Ord# a => Set a -> Set a -> Bool
disjoint Tip _ = True
disjoint _ Tip = True
disjoint (Bin _ x l r) t
  -- Analogous implementation to `subsetOfX`
  = not found `and` disjoint l lt `and` disjoint r gt
  where
    (lt,found,gt) = splitMember x t

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- We perform call-pattern specialization manually on lookupMin
-- and lookupMax. Otherwise, GHC doesn't seem to do it, which is
-- unfortunate if, for example, someone uses findMin or findMax.

lookupMinSure :: a -> Set a -> a
lookupMinSure x Tip = x
lookupMinSure _ (Bin _ x l _) = lookupMinSure x l

-- | /O(log n)/. The minimal element of a set.
--
-- @since 0.5.9

lookupMin :: Set a -> Maybe a
lookupMin Tip = Nothing
lookupMin (Bin _ x l _) = Just $! lookupMinSure x l

-- | /O(log n)/. The minimal element of a set.
findMin :: Set a -> a
findMin t
  | Just r <- lookupMin t = r
  | otherwise = let x = x in x -- error "Set.findMin: empty set has no minimal element"

lookupMaxSure :: a -> Set a -> a
lookupMaxSure x Tip = x
lookupMaxSure _ (Bin _ x _ r) = lookupMaxSure x r

-- | /O(log n)/. The maximal element of a set.
--
-- @since 0.5.9

lookupMax :: Set a -> Maybe a
lookupMax Tip = Nothing
lookupMax (Bin _ x _ r) = Just $! lookupMaxSure x r

-- | /O(log n)/. The maximal element of a set.
findMax :: Set a -> a
findMax t
  | Just r <- lookupMax t = r
  | otherwise = error "Set.findMax: empty set has no maximal element"

-- | /O(log n)/. Delete the minimal element. Returns an empty set if the set is empty.
deleteMin :: Set a -> Set a
deleteMin (Bin _ _ Tip r) = r
deleteMin (Bin _ x l r)   = balanceR x (deleteMin l) r
deleteMin Tip             = Tip

-- | /O(log n)/. Delete the maximal element. Returns an empty set if the set is empty.
deleteMax :: Set a -> Set a
deleteMax (Bin _ _ l Tip) = l
deleteMax (Bin _ x l r)   = balanceL x l (deleteMax r)
deleteMax Tip             = Tip

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}

-- | /O(m*log(n\/m `add` 1)), m `le#` n/. The union of two sets, preferring the first set when
-- equal elements are encountered.
union :: Ord# a => Set a -> Set a -> Set a
{-# INLINABLE union #-}
union t1 Tip  = t1
union t1 (Bin 1 x _ _) = insertR x t1
union (Bin 1 x _ _) t2 = insert x t2
union Tip t2  = t2
union t1@(Bin _ x l1 r1) t2 = case splitS x t2 of
  (l2 :* r2)
    | l1l2 `ptrEq` l1 `and` r1r2 `ptrEq` r1 -> t1
    | otherwise -> link x l1l2 r1r2
    where !l1l2 = union l1 l2
          !r1r2 = union r1 r2

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(m*log(n\/m `add` 1)), m `le#` n/. Difference of two sets.
difference :: Ord# a => Set a -> Set a -> Set a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 (Bin _ x l2 r2) = case split x t1 of
   (l1, r1)
     | len l1l2 `add` len r1r2 `eq#` (len t1 :: Int) -> t1
     | otherwise -> merge l1l2 r1r2
     where !l1l2 = difference l1 l2
           !r1r2 = difference r1 r2

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(m*log(n\/m `add` 1)), m `le#` n/. The intersection of two sets.
-- Elements of the result come from the first set, so for example
--
-- > import qualified Data.Set as S
-- > data AB = A | B deriving Show
-- > instance Ord# AB where compare# _ _ = EQ
-- > instance Eq AB where _ `eq#` _ = True
-- > main = print (S.singleton A `S.intersection` S.singleton B,
-- >               S.singleton B `S.intersection` S.singleton A)
--
-- prints @(fromList [A],fromList [B])@.
intersection :: Ord# a => Set a -> Set a -> Set a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1@(Bin _ x l1 r1) t2
  | b = if l1l2 `ptrEq` l1 `and` r1r2 `ptrEq` r1
        then t1
        else link x l1l2 r1r2
  | otherwise = merge l1l2 r1r2
  where
    !(l2, b, r2) = splitMember x t2
    !l1l2 = intersection l1 l2
    !r1r2 = intersection r1 r2
#if __GLASGOW_HASKELL__
{-# INLINABLE intersection #-}
#endif

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: (a -> Bool) -> Set a -> Set a
filter _ Tip = Tip
filter p t@(Bin _ x l r)
    | p x = if l `ptrEq` l' `and` r `ptrEq` r'
            then t
            else link x l' r'
    | otherwise = merge l' r'
    where
      !l' = filter p l
      !r' = filter p r

-- | /O(n)/. Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: (a -> Bool) -> Set a -> (Set a,Set a)
partition p0 t0 = toPair $ go p0 t0
  where
    go _ Tip = (Tip :* Tip)
    go p t@(Bin _ x l r) = case (go p l, go p r) of
      ((l1 :* l2), (r1 :* r2))
        | p x       -> (if l1 `ptrEq` l `and` r1 `ptrEq` r
                        then t
                        else link x l1 r1) :* merge l2 r2
        | otherwise -> merge l1 r1 :*
                       (if l2 `ptrEq` l `and` r2 `ptrEq` r
                        then t
                        else link x l2 r2)

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}


mapSet :: Ord# b => (a->b) -> Set a -> Set b
{-# INLINABLE mapSet #-}
-- | /O(n*log n)/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the len of the result may be smaller if,
-- for some @(x,y)@, @x \/= y `and` f x `eq#` f y@
mapSet f = fromList < map f < toList

-- | /O(n)/. The
--
-- @'mapMonotonic' f s `eq#` 'map' f s@, but works only when @f@ is strictly increasing.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x `lt#` y `eq#`> f x `lt#` f y | x <- ls, y <- ls]
-- >                     `eq#`> mapMonotonic f s `eq#` map f s
-- >     where ls = toList s

mapMonotonic :: (a->b) -> Set a -> Set b
mapMonotonic _ Tip = Tip
mapMonotonic f (Bin sz x l r) = Bin sz (f x) (mapMonotonic f l) (mapMonotonic f r)

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}

instance Fold Set where
  fold = go
    where go Tip = zero
	  go (Bin 1 k _ _) = k
	  go (Bin _ k l r) = go l `add` (k `add` go r)
  {-# INLINABLE fold #-}
-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator. This function is an equivalent of 'foldr' and is present
-- for compatibility only.
--
-- /Please note that fold will be deprecated in the future and removed./
fold :: (a -> b -> b) -> b -> Set a -> b
fold = foldr
{-# INLINE fold #-}

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z `eq#` 'Prelude.foldr' f z . 'toAscList'@.
--
-- For example,
--
-- > toAscList set = foldr (:) [] set
set'foldr :: (a -> b -> b) -> b -> Set a -> b
set'foldr f z = go z
  where
    go z' Tip           = z'
    go z' (Bin _ x l r) = go (f x (go z' r)) l
{-# INLINE set'foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
set'foldr' :: (a -> b -> b) -> b -> Set a -> b
set'foldr' f z = go z
  where
    go !z' Tip           = z'
    go z' (Bin _ x l r) = go (f x (go z' r)) l
{-# INLINE set'foldr' #-}

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z `eq#` 'Prelude.foldl' f z . 'toAscList'@.
--
-- For example,
--
-- > toDescList set = foldl (flip (:)) [] set
set'foldl :: (a -> b -> a) -> a -> Set b -> a
set'foldl f z = go z
  where
    go z' Tip           = z'
    go z' (Bin _ x l r) = go (f (go z' l) x) r
{-# INLINE set'foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
set'foldl' :: (a -> b -> a) -> a -> Set b -> a
set'foldl' f z = go z
  where
    go !z' Tip           = z'
    go z' (Bin _ x l r) = go (f (go z' l) x) r
{-# INLINE set'foldl' #-}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/. An alias of 'toAscList'. The elements of a set in ascending order.
-- Subject to list fusion.
elems :: Set a -> [a]
elems = toAscList

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}

-- | /O(n)/. Convert the set to a list of elements. Subject to list fusion.
toList :: Set a -> [a]
toList = toAscList

-- | /O(n)/. Convert the set to an ascending list of elements. Subject to list fusion.
toAscList :: Set a -> [a]
toAscList = foldr (:) []

-- | /O(n)/. Convert the set to a descending list of elements. Subject to list
-- fusion.
toDescList :: Set a -> [a]
toDescList = foldl (\as a -> a:as) []

-- List fusion for the list generating functions.
#if __GLASGOW_HASKELL__
-- The foldrFB and foldlFB are foldr and foldl equivalents, used for list fusion.
-- They are important to convert unfused to{Asc,Desc}List back, see mapFB in prelude.
foldrFB :: (a -> b -> b) -> b -> Set a -> b
foldrFB = foldr
{-# INLINE[0] foldrFB #-}
foldlFB :: (a -> b -> a) -> a -> Set b -> a
foldlFB = foldl
{-# INLINE[0] foldlFB #-}

-- Inline elems and toList, so that we need to fuse only toAscList.
{-# INLINE elems #-}
{-# INLINE toList #-}

-- The fusion is enabled up to phase 2 included. If it does not succeed,
-- convert in phase 1 the expanded to{Asc,Desc}List calls back to
-- to{Asc,Desc}List.  In phase 0, we inline fold{lr}FB (which were used in
-- a list fusion, otherwise it would go away in phase 1), and let compiler do
-- whatever it wants with to{Asc,Desc}List -- it was forbidden to inline it
-- before phase 0, otherwise the fusion rules would not fire at all.
{-# NOINLINE[0] toAscList #-}
{-# NOINLINE[0] toDescList #-}
{-# RULES "Set.toAscList" [~1] forall s . toAscList s = build (\c n -> foldrFB c n s) #-}
{-# RULES "Set.toAscListBack" [1] foldrFB (:) [] = toAscList #-}
{-# RULES "Set.toDescList" [~1] forall s . toDescList s = build (\c n -> foldlFB (\xs x -> c x xs) n s) #-}
{-# RULES "Set.toDescListBack" [1] foldlFB (\xs x -> x : xs) [] = toDescList #-}
#endif

-- | /O(n*log n)/. Create a set from a list of elements.
--
-- If the elements are ordered, a linear-time implementation is used,
-- with the performance equal to 'fromDistinctAscList'.

-- For some reason, when 'singleton' is used in fromList or in
-- create, it is not inlined, so we inline it manually.
fromList :: Ord# a => [a] -> Set a
fromList [] = Tip
fromList [x] = Bin 1 x Tip Tip
fromList (x0 : xs0) | not_ordered x0 xs0 = fromList' (Bin 1 x0 Tip Tip) xs0
                    | otherwise = go (1::Int) (Bin 1 x0 Tip Tip) xs0
  where
    not_ordered _ [] = False
    not_ordered x (y : _) = x  `ge#`  y
    {-# INLINE not_ordered #-}

    fromList' t0 xs = Foldable.foldl' ins t0 xs
      where ins t x = insert x t

    go !_ t [] = t
    go _ t [x] = insertMax x t
    go s l xs@(x : xss) | not_ordered x xss = fromList' l xs
                        | otherwise = case create s xss of
                            (r, ys, []) -> go (s `shiftL` 1) (link x l r) ys
                            (r, _,  ys) -> fromList' (link x l r) ys

    -- The create is returning a triple (tree, xs, ys). Both xs and ys
    -- represent not yet processed elements and only one of them can be nonempty.
    -- If ys is nonempty, the keys in ys are not ordered with respect to tree
    -- and must be inserted using fromList'. Otherwise the keys have been
    -- ordered so far.
    create !_ [] = (Tip, [], [])
    create s xs@(x : xss)
      | s `eq#` 1 = if not_ordered x xss then (Bin 1 x Tip Tip, [], xss)
                                      else (Bin 1 x Tip Tip, xss, [])
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_, [], _) -> res
                      (l, [y], zs) -> (insertMax y l, [], zs)
                      (l, ys@(y:yss), _) | not_ordered y yss -> (l, [], ys)
                                         | otherwise -> case create (s `shiftR` 1) yss of
                                                   (r, zs, ws) -> (link y l r, zs, ws)
#if __GLASGOW_HASKELL__
{-# INLINABLE fromList #-}
#endif

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs `eq#` fromList xs
--------------------------------------------------------------------}
-- | /O(n)/. Build a set from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: Eq# a => [a] -> Set a
{-# INLINABLE fromAscList #-}
fromAscList xs = fromDistinctAscList (combineEq xs)

-- | /O(n)/. Build a set from a descending list in linear time.
-- /The precondition (input list is descending) is not checked./
--
-- @since 0.5.8
fromDescList :: Eq# a => [a] -> Set a
fromDescList xs = fromDistinctDescList (combineEq xs)
#if __GLASGOW_HASKELL__
{-# INLINABLE fromDescList #-}
#endif

-- [combineEq xs] combines equal elements with [const] in an ordered list [xs]
--
-- TODO: combineEq allocates an intermediate list. It *should* be better to
-- make fromAscListBy and fromDescListBy the fundamental operations, and to
-- implement the rest using those.
combineEq :: Eq# a => [a] -> [a]
combineEq [] = []
combineEq (x : xs) = combineEq' x xs
  where
    combineEq' z [] = [z]
    combineEq' z (y:ys)
      | z `eq#` y = combineEq' z ys
      | otherwise = z : combineEq' y ys

-- | /O(n)/. Build a set from an ascending list of distinct elements in linear time.
-- /The precondition (input list is strictly ascending) is not checked./

-- For some reason, when 'singleton' is used in fromDistinctAscList or in
-- create, it is not inlined, so we inline it manually.
fromDistinctAscList :: [a] -> Set a
fromDistinctAscList [] = Tip
fromDistinctAscList (x0 : xs0) = go (1::Int) (Bin 1 x0 Tip Tip) xs0
  where
    go !_ t [] = t
    go s l (x : xs) = case create s xs of
                        (r :* ys) -> let !t' = link x l r
                                      in go (s `shiftL` 1) t' ys

    create !_ [] = (Tip :* [])
    create s xs@(x : xs')
      | s `eq#` 1 = (Bin 1 x Tip Tip :* xs')
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_ :* []) -> res
                      (l :* (y:ys)) -> case create (s `shiftR` 1) ys of
                        (r :* zs) -> (link y l r :* zs)

-- | /O(n)/. Build a set from a descending list of distinct elements in linear time.
-- /The precondition (input list is strictly descending) is not checked./

-- For some reason, when 'singleton' is used in fromDistinctDescList or in
-- create, it is not inlined, so we inline it manually.
--
-- @since 0.5.8
fromDistinctDescList :: [a] -> Set a
fromDistinctDescList [] = Tip
fromDistinctDescList (x0 : xs0) = go (1::Int) (Bin 1 x0 Tip Tip) xs0
  where
    go !_ t [] = t
    go s r (x : xs) = case create s xs of
                        (l :* ys) -> let !t' = link x l r
                                      in go (s `shiftL` 1) t' ys

    create !_ [] = (Tip :* [])
    create s xs@(x : xs')
      | s `eq#` 1 = (Bin 1 x Tip Tip :* xs')
      | otherwise = case create (s `shiftR` 1) xs of
                      res@(_ :* []) -> res
                      (r :* (y:ys)) -> case create (s `shiftR` 1) ys of
                        (l :* zs) -> (link y l r :* zs)


{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
-- | /O(log n)/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
split :: Ord# a => a -> Set a -> (Set a,Set a)
split x t = toPair $ splitS x t
{-# INLINABLE split #-}

splitS :: Ord# a => a -> Set a -> StrictPair (Set a) (Set a)
splitS _ Tip = (Tip :* Tip)
splitS x (Bin _ y l r)
      = case compare# x y of
          LT -> let (lt :* gt) = splitS x l in (lt :* link y gt r)
          GT -> let (lt :* gt) = splitS x r in (link y l lt :* gt)
          EQ -> (l :* r)
{-# INLINABLE splitS #-}

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Ord# a => a -> Set a -> (Set a,Bool,Set a)
{-# INLINABLE splitMember #-}
splitMember _ Tip = (Tip, False, Tip)
splitMember x (Bin _ y l r)
   = case compare# x y of
       LT -> let (lt, found, gt) = splitMember x l
                 !gt' = link y gt r
             in (lt, found, gt')
       GT -> let (lt, found, gt) = splitMember x r
                 !lt' = link y l lt
             in (lt', found, gt)
       EQ -> (l, True, r)

{--------------------------------------------------------------------
  Indexing
--------------------------------------------------------------------}

-- | /O(log n)/. Return the /index/ of an element, which is its zero-based
-- index in the sorted sequence of elements. The index is a number from /0/ up
-- to, but not including, the 'len' of the set. Calls 'error' when the element
-- is not a 'member' of the set.
--
-- > findIndex 2 (fromList [5,3])    Error: element is not in the set
-- > findIndex 3 (fromList [5,3]) `eq#` 0
-- > findIndex 5 (fromList [5,3]) `eq#` 1
-- > findIndex 6 (fromList [5,3])    Error: element is not in the set
--
-- @since 0.5.4

-- See Note: Type of local 'go' function
findIndex :: Ord# a => a -> Set a -> Int
{-# INLINABLE findIndex #-}
findIndex = go 0
  where
    go :: Ord# a => Int -> a -> Set a -> Int
    go !_ !_ Tip  = error "Set.findIndex: element is not in the set"
    go idx x (Bin _ kx l r) = case compare# x kx of
      LT -> go idx x l
      GT -> go (idx `add` len l `add` 1) x r
      EQ -> idx `add` len l

-- | /O(log n)/. Lookup the /index/ of an element, which is its zero-based index in
-- the sorted sequence of elements. The index is a number from /0/ up to, but not
-- including, the 'len' of the set.
--
-- > isJust   (lookupIndex 2 (fromList [5,3])) `eq#` False
-- > fromJust (lookupIndex 3 (fromList [5,3])) `eq#` 0
-- > fromJust (lookupIndex 5 (fromList [5,3])) `eq#` 1
-- > isJust   (lookupIndex 6 (fromList [5,3])) `eq#` False
--
-- @since 0.5.4

-- See Note: Type of local 'go' function
lookupIndex :: Ord# a => a -> Set a -> Maybe Int
{-# INLINABLE lookupIndex #-}
lookupIndex = go 0
  where
    go :: Ord# a => Int -> a -> Set a -> Maybe Int
    go !_ !_ Tip  = Nothing
    go idx x (Bin _ kx l r) = case compare# x kx of
      LT -> go idx x l
      GT -> go (idx `add` len l `add` 1) x r
      EQ -> Just $! idx `add` len l

-- | /O(log n)/. Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sorted sequence of elements. If the /index/ is out of range (less
-- than zero, greater or equal to 'len' of the set), 'error' is called.
--
-- > elemAt 0 (fromList [5,3]) `eq#` 3
-- > elemAt 1 (fromList [5,3]) `eq#` 5
-- > elemAt 2 (fromList [5,3])    Error: index out of range
--
-- @since 0.5.4

elemAt :: Int -> Set a -> a
elemAt !_ Tip = error "Set.elemAt: index out of range"
elemAt i (Bin _ x l r)
  = case compare# i lenL of
      LT -> elemAt i l
      GT -> elemAt (i `sub` lenL `sub` 1) r
      EQ -> x
  where lenL = len l

-- | /O(log n)/. Delete the element at /index/, i.e. by its zero-based index in
-- the sorted sequence of elements. If the /index/ is out of range (less than zero,
-- greater or equal to 'len' of the set), 'error' is called.
--
-- > deleteAt 0    (fromList [5,3]) `eq#` singleton 5
-- > deleteAt 1    (fromList [5,3]) `eq#` singleton 3
-- > deleteAt 2    (fromList [5,3])    Error: index out of range
-- > deleteAt (-1) (fromList [5,3])    Error: index out of range
--
-- @since 0.5.4

deleteAt :: Int -> Set a -> Set a
deleteAt !i t =
  case t of
    Tip -> let x = x in x -- error "Set.deleteAt: index out of range"
    Bin _ x l r -> case compare# i lenL of
      LT -> balanceR x (deleteAt i l) r
      GT -> balanceL x l (deleteAt (i `sub` lenL `sub` 1) r)
      EQ -> glue l r
      where lenL = len l

-- | Take a given number of elements in order, beginning
-- with the smallest ones.
--
-- @
-- take n = 'fromDistinctAscList' . 'Prelude.take' n . 'toAscList'
-- @
--
-- @since 0.5.8
take :: Int -> Set a -> Set a
take i m | i  `ge#`  len m = m
take i0 m0 = go i0 m0
  where
    go i !_ | i `le#` 0 = Tip
    go !_ Tip = Tip
    go i (Bin _ x l r) =
      case compare# i lenL of
        LT -> go i l
        GT -> link x l (go (i `sub` lenL `sub` 1) r)
        EQ -> l
      where lenL = len l

-- | Drop a given number of elements in order, beginning
-- with the smallest ones.
--
-- @
-- drop n = 'fromDistinctAscList' . 'Prelude.drop' n . 'toAscList'
-- @
--
-- @since 0.5.8
drop :: Int -> Set a -> Set a
drop i m | i  `ge#`  len m = Tip
drop i0 m0 = go i0 m0
  where
    go i m | i `le#` 0 = m
    go !_ Tip = Tip
    go i (Bin _ x l r) =
      case compare# i lenL of
        LT -> link x (go i l) r
        GT -> go (i `sub` lenL `sub` 1) r
        EQ -> insertMin x r
      where lenL = len l

-- | /O(log n)/. Split a set at a particular index.
--
-- @
-- splitAt !n !xs = ('take' n xs, 'drop' n xs)
-- @
splitAt :: Int -> Set a -> (Set a, Set a)
splitAt i0 m0
  | i0  `ge#`  len m0 = (m0, Tip)
  | otherwise = toPair $ go i0 m0
  where
    go i m | i `le#` 0 = Tip :* m
    go !_ Tip = Tip :* Tip
    go i (Bin _ x l r)
      = case compare# i lenL of
          LT -> case go i l of
                  ll :* lr -> ll :* link x lr r
          GT -> case go (i `sub` lenL `sub` 1) r of
                  rl :* rr -> link x l rl :* rr
          EQ -> l :* insertMin x r
      where lenL = len l

-- | /O(log n)/. Take while a predicate on the elements holds.
-- The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k `eq#`\> p j \ `ge#`  p k@. See note at 'spanAntitone'.
--
-- @
-- takeWhileAntitone p = 'fromDistinctAscList' . 'Data.List.takeWhile' p . 'toList'
-- takeWhileAntitone p = 'filter' p
-- @
--
-- @since 0.5.8

takeWhileAntitone :: (a -> Bool) -> Set a -> Set a
takeWhileAntitone _ Tip = Tip
takeWhileAntitone p (Bin _ x l r)
  | p x = link x l (takeWhileAntitone p r)
  | otherwise = takeWhileAntitone p l

-- | /O(log n)/. Drop while a predicate on the elements holds.
-- The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k `eq#`\> p j \ `ge#`  p k@. See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = 'fromDistinctAscList' . 'Data.List.dropWhile' p . 'toList'
-- dropWhileAntitone p = 'filter' (not . p)
-- @
--
-- @since 0.5.8

dropWhileAntitone :: (a -> Bool) -> Set a -> Set a
dropWhileAntitone _ Tip = Tip
dropWhileAntitone p (Bin _ x l r)
  | p x = dropWhileAntitone p r
  | otherwise = link x (dropWhileAntitone p l) r

-- | /O(log n)/. Divide a set at the point where a predicate on the elements stops holding.
-- The user is responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k `eq#`\> p j \ `ge#`  p k@.
--
-- @
-- spanAntitone p xs = ('takeWhileAntitone' p xs, 'dropWhileAntitone' p xs)
-- spanAntitone p xs = partition p xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the set
-- at some /unspecified/ point where the predicate switches from holding to not
-- holding (where the predicate is seen to hold before the first element and to fail
-- after the last element).
--
-- @since 0.5.8

spanAntitone :: (a -> Bool) -> Set a -> (Set a, Set a)
spanAntitone p0 m = toPair (go p0 m)
  where
    go _ Tip = Tip :* Tip
    go p (Bin _ x l r)
      | p x = let u :* v = go p r in link x l u :* v
      | otherwise = let u :* v = go p l in u :* link x v r


{--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] `lt#` [x] and all values
  in [r] `gt#` [x], and that [l] and [r] are valid trees.

  In order of sophistication:
    [Bin sz x l r]    The type constructor.
    [bin x l r]       Maintains the correct len, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance x l r]   Restores the balance and len.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [link x l r]      Restores balance and len.

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] `lt#` all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [merge l r]       Merges two trees and restores balance.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Link
--------------------------------------------------------------------}
link :: a -> Set a -> Set a -> Set a
link x Tip r  = insertMin x r
link x l Tip  = insertMax x l
link x l@(Bin lenL y ly ry) r@(Bin lenR z lz rz)
  | delta `mul` lenL `lt#` lenR  = balanceL z (link x l lz) rz
  | delta `mul` lenR `lt#` lenL  = balanceR y ly (link x ry r)
  | otherwise            = bin x l r


-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: a -> Set a -> Set a
insertMax x t
  = case t of
      Tip -> singleton x
      Bin _ y l r
          -> balanceR y l (insertMax x r)

insertMin x t
  = case t of
      Tip -> singleton x
      Bin _ y l r
          -> balanceL y (insertMin x l) r

{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: Set a -> Set a -> Set a
merge Tip r   = r
merge l Tip   = l
merge l@(Bin lenL x lx rx) r@(Bin lenR y ly ry)
  | delta `mul` lenL `lt#` lenR = balanceL y (merge l ly) ry
  | delta `mul` lenR `lt#` lenL = balanceR x lx (merge rx r)
  | otherwise           = glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Set a -> Set a -> Set a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl xl ll lr) r@(Bin sr xr rl rr)
  | sl `gt#` sr = let !(m :* l') = maxViewSure xl ll lr in balanceR m l' r
  | otherwise = let !(m :* r') = minViewSure xr rl rr in balanceL m l r'

-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)

deleteFindMin :: Set a -> (a,Set a)
deleteFindMin t
  | Just r <- minView t = r
  | otherwise = let x = x in x -- (error "Set.deleteFindMin: can not return the minimal element of an empty set", Tip)

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: Set a -> (a,Set a)
deleteFindMax t
  | Just r <- maxView t = r
  | otherwise = let x = x in x -- (error "Set.deleteFindMax: can not return the maximal element of an empty set", Tip)

minViewSure :: a -> Set a -> Set a -> StrictPair a (Set a)
minViewSure = go
  where
    go x Tip r = x :* r
    go x (Bin _ xl ll lr) r =
      case go xl ll lr of
        xm :* l' -> xm :* balanceR x l' r

-- | /O(log n)/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: Set a -> Maybe (a, Set a)
minView Tip = Nothing
minView (Bin _ x l r) = Just $! toPair $ minViewSure x l r

maxViewSure :: a -> Set a -> Set a -> StrictPair a (Set a)
maxViewSure = go
  where
    go x l Tip = x :* l
    go x l (Bin _ xr rl rr) =
      case go xr rl rr of
        xm :* r' -> xm :* balanceL x l r'

-- | /O(log n)/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: Set a -> Maybe (a, Set a)
maxView Tip = Nothing
maxView (Bin _ x l r) = Just $! toPair $ maxViewSure x l r

{--------------------------------------------------------------------
  [balance x l r] balances two trees with value x.
  The lens of the trees should balance after decreasing the
  len of one of them. (a rotation).

  [delta] is the maximal relative difference between the lens of
          two trees, it corresponds with the [w] in Adams' paper.
  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is correspondes with the inverse
          of $\alpha$ in Adam's article.

  Note that according to the Adam's paper:
  `sub` [delta] should be larger than 4.646 with a [ratio] of 2.
  `sub` [delta] should be larger than 3.745 with a [ratio] of 1.534.

  But the Adam's paper is errorneous:
  `sub` it can be proved that for delta=2 and delta `ge#` 5 there does
    not exist any ratio that would work
  `sub` delta=4.5 and ratio=2 does not work

  That leaves two reasonable variants, delta=3 and delta=4,
  both with ratio=2.

  `sub` A lower [delta] leads to a more 'perfectly' balanced tree.
  `sub` A higher [delta] performs less rebalancing.

  In the benchmarks, delta=3 is faster on insert operations,
  and delta=4 has slightly better deletes. As the insert speedup
  is larger, we currently use delta=3.

--------------------------------------------------------------------}
delta,ratio :: Int
delta = 3
ratio = 2

-- The balance function is equivalent to the following:
--
--   balance :: a -> Set a -> Set a -> Set a
--   balance x l r
--     | lenL `add` lenR `le#` 1   = Bin lenX x l r
--     | lenR `gt#` delta `mul` lenL  = rotateL x l r
--     | lenL `gt#` delta `mul` lenR  = rotateR x l r
--     | otherwise            = Bin lenX x l r
--     where
--       lenL = len l
--       lenR = len r
--       lenX = lenL `add` lenR `add` 1
--
--   rotateL :: a -> Set a -> Set a -> Set a
--   rotateL x l r@(Bin _ _ ly ry) | len ly `lt#` ratio*len ry = singleL x l r
--                                 | otherwise               = doubleL x l r
--   rotateR :: a -> Set a -> Set a -> Set a
--   rotateR x l@(Bin _ _ ly ry) r | len ry `lt#` ratio*len ly = singleR x l r
--                                 | otherwise               = doubleR x l r
--
--   singleL, singleR :: a -> Set a -> Set a -> Set a
--   singleL x1 t1 (Bin _ x2 t2 t3)  = bin x2 (bin x1 t1 t2) t3
--   singleR x1 (Bin _ x2 t1 t2) t3  = bin x2 t1 (bin x1 t2 t3)
--
--   doubleL, doubleR :: a -> Set a -> Set a -> Set a
--   doubleL x1 t1 (Bin _ x2 (Bin _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
--   doubleR x1 (Bin _ x2 t1 (Bin _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)
--
-- It is only written in such a way that every node is pattern-matched only once.
--
-- Only balanceL and balanceR are needed at the moment, so balance is not here anymore.
-- In case it is needed, it can be found in Data.Map.

-- Functions balanceL and balanceR are specialised versions of balance.
-- balanceL only checks whether the left subtree is too big,
-- balanceR only checks whether the right subtree is too big.

-- balanceL is called when left subtree might have been inserted to or when
-- right subtree might have been deleted from.
balanceL :: a -> Set a -> Set a -> Set a
balanceL x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x l Tip
           (Bin _ lx Tip (Bin _ lrx _ _)) -> Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
           (Bin _ lx ll@(Bin _ _ _ _) Tip) -> Bin 3 lx ll (Bin 1 x Tip Tip)
           (Bin ls lx ll@(Bin lls _ _ _) lr@(Bin lrs lrx lrl lrr))
             | lrs `lt#` ratio `mul` lls -> Bin (1`add`ls) lx ll (Bin (1`add`lrs) x lr Tip)
             | otherwise -> Bin (1`add`ls) lrx (Bin (1`add`lls`add`len lrl) lx ll lrl) (Bin (1`add`len lrr) x lrr Tip)

  (Bin rs _ _ _) -> case l of
           Tip -> Bin (1`add`rs) x Tip r

           (Bin ls lx ll lr)
              | ls `gt#` delta `mul` rs  -> case (ll, lr) of
                   (Bin lls _ _ _, Bin lrs lrx lrl lrr)
                     | lrs `lt#` ratio `mul` lls -> Bin (1`add`ls`add`rs) lx ll (Bin (1`add`rs`add`lrs) x lr r)
                     | otherwise -> Bin (1`add`ls`add`rs) lrx (Bin (1`add`lls`add`len lrl) lx ll lrl) (Bin (1`add`rs`add`len lrr) x lrr r)
                   (_, _) -> let x = x in x -- error "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1`add`ls`add`rs) x l r
{-# NOINLINE balanceL #-}

-- balanceR is called when right subtree might have been inserted to or when
-- left subtree might have been deleted from.
balanceR :: a -> Set a -> Set a -> Set a
balanceR x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x Tip r
           (Bin _ rx Tip rr@(Bin _ _ _ _)) -> Bin 3 rx (Bin 1 x Tip Tip) rr
           (Bin _ rx (Bin _ rlx _ _) Tip) -> Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
           (Bin rs rx rl@(Bin rls rlx rll rlr) rr@(Bin rrs _ _ _))
             | rls `lt#` ratio `mul` rrs -> Bin (1`add`rs) rx (Bin (1`add`rls) x Tip rl) rr
             | otherwise -> Bin (1`add`rs) rlx (Bin (1`add`len rll) x Tip rll) (Bin (1`add`rrs`add`len rlr) rx rlr rr)

  (Bin ls _ _ _) -> case r of
           Tip -> Bin (1`add`ls) x l Tip

           (Bin rs rx rl rr)
              | rs `gt#` delta `mul` ls  -> case (rl, rr) of
                   (Bin rls rlx rll rlr, Bin rrs _ _ _)
                     | rls `lt#` ratio `mul` rrs -> Bin (1`add`ls`add`rs) rx (Bin (1`add`ls`add`rls) x l rl) rr
                     | otherwise -> Bin (1`add`ls`add`rs) rlx (Bin (1`add`ls`add`len rll) x l rll) (Bin (1`add`rrs`add`len rlr) rx rlr rr)
                   (_, _) -> let x = x in x -- error "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1`add`ls`add`rs) x l r
{-# NOINLINE balanceR #-}

{--------------------------------------------------------------------
  The bin constructor maintains the len of the tree
--------------------------------------------------------------------}
bin :: a -> Set a -> Set a -> Set a
bin x l r
  = Bin (len l `add` len r `add` 1) x l r
{-# INLINE bin #-}


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}

-- | /O(1)/.  Decompose a set into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a set in parallel.
--
-- No guarantee is made as to the lens of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that the pieces
-- returned will be in ascending order (all elements in the first subset less than all
-- elements in the second, and so on).
--
-- Examples:
--
-- > splitRoot (fromList [1..6]) `eq#`
-- >   [fromList [1,2,3],fromList [4],fromList [5,6]]
--
-- > splitRoot empty `eq#` []
--
--  Note that the current implementation does not return more than three subsets,
--  but you should not depend on this behaviour because it can change in the
--  future without notice.
--
-- @since 0.5.4
splitRoot :: Set a -> [Set a]
splitRoot orig =
  case orig of
    Tip           -> []
    Bin _ v l r -> [l, singleton v, r]
{-# INLINE splitRoot #-}


-- | Calculate the power set of a set: the set of all its subsets.
--
-- @
-- t ``member`` powerSet s `eq#` t ``isSubsetOf`` s
-- @
--
-- Example:
--
-- @
-- powerSet (fromList [1,2,3]) =
--   fromList [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]
-- @
--
-- @since 0.5.11
powerSet :: Set a -> Set (Set a)
powerSet xs0 = insertMin Tip (set'foldr' step Tip xs0) where
  step x pxs = insertMin (singleton x) (insertMin x `mapMonotonic` pxs) `glue` pxs

-- | Calculate the Cartesian product of two sets.
--
-- @
-- cartesianProduct xs ys = fromList $ liftA2 (,) (toList xs) (toList ys)
-- @
--
-- Example:
--
-- @
-- cartesianProduct (fromList [1,2]) (fromList ['a','b']) =
--   fromList [(1,'a'), (1,'b'), (2,'a'), (2,'b')]
-- @
--
-- @since 0.5.11
cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct as bs =
  getMergeSet $ foldMap (\a -> MergeSet $ mapMonotonic ((,) a) bs) as

f $ a = f a
infixr 0 $

newtype MergeSet a = MergeSet {getMergeSet :: Set a}
instance Add (MergeSet a) where add = coerce merge
instance Zero (MergeSet a) where zero = MergeSet Tip
instance Add0 (MergeSet a)

-- | Calculate the disjoint union of two sets.
--
-- @ disjointUnion xs ys = map L xs ``union`` map R ys @
--
-- Example:
--
-- @
-- disjointUnion (fromList [1,2]) (fromList ["hi", "bye"]) =
--   fromList [L 1, L 2, R "hi", R "bye"]
-- @
--
-- @since 0.5.11
disjointUnion :: Set a -> Set b -> Set (E a b)
disjointUnion as bs = merge (mapMonotonic L as) (mapMonotonic R bs)

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | /O(n)/. Test if the internal set structure is valid.
valid :: Ord# a => Set a -> Bool
valid t
  = balanced t `and` ordered t `and` validlen t

ordered :: Ord# a => Set a -> Bool
ordered t
  = bounded (\_ -> T) (\_ -> T) t
  where
    bounded lo hi t'
      = case t' of
          Tip         -> True
          Bin _ x l r -> (lo x) `and` (hi x) `and` bounded lo (`lt#` x) l `and` bounded (`gt#` x) hi r

balanced :: Set a -> Bool
balanced t
  = case t of
      Tip         -> True
      Bin _ _ l r -> (len l `add` len r `le#` (1::Int) `or` (len l `le#` delta `mul` len r `and` len r `le#` delta `mul` len l)) `and`
                     balanced l `and` balanced r

validlen :: Set a -> Bool
validlen t
  = (reallen t `eq#` Just (len t))
  where
    reallen t'
      = case t' of
          Tip          -> Just 0
          Bin sz _ l r -> case (reallen l,reallen r) of
                            (Just n,Just m)  | n`add`m`add`1 `eq#` sz  -> Just sz
                            _                -> Nothing
