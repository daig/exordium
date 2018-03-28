module X.Functor.Align (module X.Functor.Align, Add, module X) where
import X.Num.Add
import X.Functor.Map as X
import X.Data.Maybe
import {-# source #-} X.Data.These as X

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
alignWith_add :: (Align f, Add a) => f a -> f a -> f a
alignWith_add = alignWith (\x -> x) (\x -> x) add 


{-fmax :: f a -> f b -> f (These a b)-}
{-fmin :: f a -> f b -> f (a,b)-}
{-fadd :: f a -> f b -> f (E a b)-}
{-fmul :: f a -> f b -> f (a,b)-}


prod as bs = [(a,b) | a <- as, b <- bs]
append [] bs = bs
append (a:as) bs = a : append as bs

bind :: (a -> [b]) -> [a] -> [b]
bind f [] = []
bind f (a:as) = append (f a) (bind f as)
bind_ap abs as = bind (\a -> map (, a) abs) as

--

-- https://hackage.haskell.org/package/unamb
instance Align Maybe where
  alignWith ar br abr = go where
    go Nothing Nothing = Nothing
    go Nothing (Just b) = Just (br b)
    go (Just a) Nothing = Just (ar a)
    go (Just a) (Just b) = Just (abr a b)
