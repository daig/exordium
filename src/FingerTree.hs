{-# language UndecidableInstances #-}
{-# language MagicHash #-}
module FingerTree (FingerTree(..,FTN), module X) where
import Measured.Class as X
import FingerTree.Digit as X
import FingerTree.Node as X
import Prelude (Show,error)
import Eq
import Unsafe
import Cons
import Snoc
import Prism
import Optic.Review
import Bool.Type

data FingerTree a
  = FT0
  | FT1 ~a
  | FTN# (Measure a) (Digit a) ~(FingerTree (Node a)) (Digit a)
deriving instance (Show a, Show (Measure a)) => Show (FingerTree a)
instance (Measured a, Zero (Measure a)) => Measured (FingerTree a) where
  type Measure (FingerTree a) = Measure a
  measure = \case
    FT0 -> zero
    FT1 a -> measure a
    FTN# v _ _ _ -> v

instance FoldMap FingerTree where
  foldMap f = \case
    FT0 -> zero
    FT1 x -> f x
    FTN# _ l t r -> foldMap f l `plus` foldMap (foldMap f) t `plus` foldMap f r


pattern FTN :: Measured a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
pattern FTN l t r <- FTN# _ l t r where
  FTN l t r = FTN# ((measure l `plus` measure t) `plus` measure r) l t r

{-# complete FT0,FT1,FTN #-}


nodeToDigit :: Node a -> Digit a
nodeToDigit = \case
  Node2# _ a b -> Digit2 a b
  Node3# _ a b c -> Digit3 a b c




-- | Only safe if function preserves measure.
instance Map (Unsafe FingerTree) where
  map f (Unsafe ft) = Unsafe (go ft) where
    go FT0 = FT0
    go (FT1 x) = FT1 (f x)
    go (FTN# v pr m sf) =
        FTN# (coerce# v) (map f pr) (mapAs# @(Unsafe FingerTree) (mapAs# @(Unsafe Node) f) m) (map f sf)
-- | Only safe if function preserves measure.
instance MapIso (Unsafe FingerTree) where mapIso _ = map



-- Views

_Digit :: (Measured a, Prism p) => p (Digit a) (Digit a) -> p (FingerTree a) (FingerTree a)
_Digit = prism treeToDigit digitToTree where
  digitToTree = \case
    Digit1 a -> FT1 a
    Digit2 a b -> FTN (Digit1 a) FT0 (Digit1 b)
    Digit3 a b c -> FTN (Digit2 a b) FT0 (Digit1 c)
    Digit4 a b c d -> FTN (Digit2 a b) FT0 (Digit2 c d)
  treeToDigit = \case
    FT1 a -> R (Digit1 a)
    FTN (Digit1 a) FT0 (Digit1 b) -> R (Digit2 a b)
    FTN (Digit2 a b) FT0 (Digit1 c) -> R (Digit3 a b c)
    FTN (Digit2 a b) FT0 (Digit2 c d) -> R (Digit4 a b c d)
    -- TODO: make sure these are actually representable
    --     ,or remove with a note if they can be ruled out
    FTN (Digit1 a) FT0 (Digit2 b c) -> R (Digit3 a b c)
    FTN (Digit1 a) (FT1 (Node2 b c)) (Digit1 d) -> R (Digit4 a b c d)
    t -> L t


rotL :: Measured a => FingerTree (Node a) -> Digit a -> FingerTree a
rotL m sf      =   case viewl m of
    EmptyL  ->  review _Digit sf
    ViewL a m' ->  FTN# (measure m `plus` measure sf) (nodeToDigit a) m' sf

data ViewL a = EmptyL | ViewL a (FingerTree a)

-- | /O(1)/. Analyse the left end of a sequence.
viewl :: Measured a => FingerTree a -> ViewL a
viewl FT0                     =  EmptyL
viewl (FT1 x)                =  x `ViewL` FT0
viewl (FTN (Digit1 x) m sf)     =  x `ViewL` rotL m sf
viewl (FTN pr m sf)          =  lheadDigit pr `ViewL` FTN (ltailDigit pr) m sf

instance (Measured a,Measured b) => Cons (FingerTree a) a b (FingerTree b) where
  _Cons = prism viewl' (\(b,t) -> ftCons b t)
    where
      viewl' = \case
        FT0 -> L FT0
        FT1 x -> R (x,FT0)
        FTN (Digit1 x) m sf -> R (x,rotL m sf)
        FTN pr m sf -> R (lheadDigit pr, FTN (ltailDigit pr) m sf)
      ftCons :: Measured x => x -> FingerTree x -> FingerTree x
      ftCons a = \case
        FT0 -> FT1 a
        FT1 b -> FTN (Digit1 a) FT0 (Digit1 b)
        FTN# v (Digit4 b c d e) !t r -> FTN# (measure a `plus` v) (Digit2 a b) (Node3 c d e `ftCons` t) r
        FTN# v l t r -> FTN# (measure a `plus` v) (consDigit# l) t r
        where
          consDigit# = \case
            Digit1 b -> Digit2 a b
            Digit2 b c -> Digit3 a b c
            Digit3 b c d -> Digit4 a b c d
            Digit4{} -> error "consDigit#"
instance (Measured a, Measured b) => Snoc (FingerTree a) a b (FingerTree b) where
  _Snoc = prism viewr' (\(t,b) -> ftSnoc t b)
    where
      viewr' = \case
        FT0 -> L FT0
        FT1 x -> R (FT0,x)
        FTN pr m (Digit1 x) -> R (rotR pr m,x)
        FTN pr m sf -> R (FTN pr m (rtailDigit sf), rheadDigit sf)
      ftSnoc :: Measured x => FingerTree x -> x -> FingerTree x
      ftSnoc x a = case x of
          FT0 -> FT1 a
          FT1 b -> FTN (Digit1 b) FT0 (Digit1 a)
          FTN# v l !t (Digit4 b c d e) -> FTN# (v `plus` measure a) l (t `ftSnoc` Node3 b c d) (Digit2 e a)
          FTN# v l t r -> FTN# (v `plus` measure a) l t (snocDigit# r)
          where
            snocDigit# = \case
              Digit1 b -> Digit2 b a
              Digit2 b c -> Digit3 b c a
              Digit3 b c d -> Digit4 b c d a
              Digit4{} -> error "snocDigit#"

data ViewR a = EmptyR | ViewR (FingerTree a) a
-- | /O(1)/. Analyse the right end of a sequence.
viewr :: Measured a => FingerTree a -> ViewR a
viewr = \case
 FT0 -> EmptyR
 FT1 x ->  FT0 `ViewR` x
 FTN pr m (Digit1 x) -> rotR pr m `ViewR` x
 FTN pr m sf -> FTN pr m (rtailDigit sf) `ViewR` rheadDigit sf
rotR :: Measured a => Digit a -> FingerTree (Node a) -> FingerTree a
rotR pr m = case viewr m of
    EmptyR  ->  review _Digit pr
    ViewR m' a ->  FTN# (measure pr `plus` measure m) pr m' (nodeToDigit a)


splitNode :: Measured a => (Measure a -> Bool) -> Measure a -> Node a
          -> (Maybe (Digit a), a, Maybe (Digit a))
splitNode p i = \case
  Node2 a b | p va -> (Nothing, a, Just (Digit1 b))
            | True -> (Just (Digit1 a), b, Nothing)
    where va = i `plus` measure a
  Node3 a b c | p va -> (Nothing, a, Just (Digit2 b c))
              | p vab -> (Just (Digit1 a), b, Just (Digit1 c))
              | True -> (Just (Digit2 a b), c, Nothing)
    where (va,vab) = (i `plus` measure a, va `plus` measure b)



splitTree :: forall a. Measured a => (Measure a -> Bool) -> Measure a -> FingerTree a
          -> (FingerTree a, a, FingerTree a)
splitTree p i = \case
  FT0 -> error "splitTree FT0"
  FT1 x -> (FT0,x,FT0)
  FTN pr m sf | p vpr -> let (l, x, r) = splitDigit p i pr
                         in (maybe FT0 (review _Digit) l, x, ftL r m sf)
              | p vm -> let  (ml, xs, mr)  =  splitTree p vpr m
                             (l, x, r)     =  splitNode p (vpr `plus` measure ml) xs
                        in   (ftR pr ml l, x, ftL r mr sf)
              | True -> let (l,x,r) = splitDigit p vm sf
                        in (ftR pr m l, x, maybe FT0 (review _Digit) r)
    where (vpr,vm) = (i `plus` measure pr, vpr `plus` measure m)

-- | Rotate if nothing on the left
ftL :: Measured a => Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
ftL pr' m sf = maybe (rotL m sf) (\pr -> FTN pr m sf) pr'

-- | Rotate if nothing on the right
ftR :: Measured a => Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
ftR pr m = maybe (rotR pr m) (FTN pr m)

-- | /O(log(min(i,n-i)))/. Split a sequence at a point where the predicate
-- on the accumulated measure of the prefix changes from 'False' to 'True'.
--
-- For predictable results, one should ensure that there is only one such
-- point, i.e. that the predicate is /monotonic/.
split ::  Measured a => (Measure a -> Bool) -> FingerTree a -> (FingerTree a, FingerTree a)
split p = \case
  FT0 -> (FT0,FT0)
  xs | p (measure xs) -> let (l,x,r) = splitTree p zero xs in (l, x :< r)
  xs -> (xs,FT0)
