module Optic.Re where
import Arrow.Closed as X
import Arrow.Traversed as X
import Functor.Swap -- TODO: integrate into E
import Prelude ((.))
import Functor.Bifold

newtype Re p s t a b = Re {runRe :: p b a -> p t s}

instance Promap p => Promap (Re p s t) where
  promap f g (Re l) = Re (\p -> l (promap g f p))

_Re :: Promap w => w (Re p s t a b) (Re q s' t' a' b') -> w (p b a -> p t s) (q b' a' -> q t' s')
_Re = promap Re runRe
re :: (Re q s t s t -> Re p s t a b) -> p b a -> p t s
re = (`_Re` (\q -> q))

-- TODO: should it use Cochoice, or some 'EmptyP' class?
instance Cochoice p => Traversed' (Re p s t) where
  _R (Re l) = Re (\p -> l (un_R p))
instance Traversed' p => Cochoice (Re p s t) where
  un_R (Re l) = Re (\p -> l (_R p))
instance Cochoice (->) where
  un_L f = go . L where go = bifoldMap_ (\x -> x) (go . R) . f
  un_R f = go . R where go = bifoldMap_ (go . L) (\x -> x) . f
