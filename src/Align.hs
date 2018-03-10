module Align (module Align, Plus, module X) where
import Plus
import Map as X
import {-# source #-} These as X

-- | fa |&| fb = map swap (fb |&| fa) TODO: Is this always useful?
-- f |&| empty = map This f
-- empty |&| g = map That g
class Map f => Align f where
  {-# minimal alignWith | align #-}
  align :: f a -> f b -> f (These a b)
  align = alignWith This That These
  alignWith :: (a -> c) -> (b -> c) -> (a -> b -> c) -> f a -> f b -> f c
  alignWith f g h = \a b -> go `map` align a b where
    go = \case
      This x -> f x
      That y -> g y
      These x y -> h x y
  {-both :: (a -> b -> c) -> -}

-- | Default definition for (+) @(f a)
alignWith_plus :: (Align f, Plus a) => f a -> f a -> f a
alignWith_plus = alignWith (\x -> x) (\x -> x) plus 


{-fmax :: f a -> f b -> f (These a b)-}
{-fmin :: f a -> f b -> f (a,b)-}
{-fplus :: f a -> f b -> f (E a b)-}
{-ftimes :: f a -> f b -> f (a,b)-}


prod as bs = [(a,b) | a <- as, b <- bs]
append [] bs = bs
append (a:as) bs = a : append as bs

bind :: (a -> [b]) -> [a] -> [b]
bind f [] = []
bind f (a:as) = append (f a) (bind f as)
bind_ap abs as = bind (\a -> map (, a) abs) as

-- map (\a -> fmin (ftop a) bs) as
-- These (f a) (f b) -> f (These a b)
--
class Never
data List a where
  Nil :: List a
  Cons :: a -> List a -> List a
  Mu :: Never => List a
instance Map List where map f = \case {Nil -> Nil; Cons a as -> Cons (f a) (map f as); Mu -> Mu}
instance MapIso List where mapIso _ = map
instance Align List where
  align (Cons a as) (Cons b bs) = Cons (These a b) (align as bs)
  align as Nil = map This as
  align Nil bs = map That bs
  align as Mu = map This as
  align Mu bs = map That bs

-- https://hackage.haskell.org/package/unamb
