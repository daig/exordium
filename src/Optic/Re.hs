module Optic.Re where
import Traversed as X
import Swap -- TODO: integrate into E
import Prelude ((.))
import FoldMap.Bi

newtype Re p s t a b = Re {runRe :: p b a -> p t s}

instance MapR p => ComapL (Re p s t) where colmap f (Re l) = Re (\p -> l (rmap f p))
instance ComapL p => MapR (Re p s t) where rmap f (Re l) = Re (\p -> l (colmap f p))
instance Promap p => Promap (Re p s t) where
  promap f g (Re l) = Re (\p -> l (promap g f p))

_Re :: Promap w => w (Re p s t a b) (Re q s' t' a' b') -> w (p b a -> p t s) (q b' a' -> q t' s')
_Re = promap Re runRe
re :: (Re q s t s t -> Re p s t a b) -> p b a -> p t s
re = (`_Re` (\q -> q))

instance Cochoice p => Traversed' (Re p s t) where
  right (Re l) = Re (\p -> l (unright p))
instance Cochoice (->) where
  unleft f = go . L where go = bifoldMap_ (\x -> x) (go . R) . f
  unright f = go . R where go = bifoldMap_ (go . L) (\x -> x) . f
