module Optic.Re where
import Arrow.Closed as X
import Arrow.Loop as X
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

-- TODO: should it use Loop', or some 'EmptyP' class?
instance Loop' p => Traversed' (Re p s t) where
  _R (Re l) = Re (\p -> l (loopRight p))
instance Traversed' p => Loop' (Re p s t) where
  loopRight (Re l) = Re (\p -> l (_R p))
instance Loop' (->) where
  loopLeft f = go . L where go = bifoldMap_ (\x -> x) (go . R) . f
  loopRight f = go . R where go = bifoldMap_ (go . L) (\x -> x) . f
