module X.Optic.Re (module X.Optic.Re, module X) where
import X.Optic.Iso as X

newtype Re p s t a b = Re {runRe :: p b a -> p t s}

instance Promap p => Promap (Re p s t) where
  promap f g (Re l) = Re (\p -> l (promap g f p))

_Re :: Promap w => w (Re p s t a b) (Re q s' t' a' b') -> w (p b a -> p t s) (q b' a' -> q t' s')
_Re = promap Re runRe
-- | Turn around an optic, getting the corresponding opposite optice.
-- an inverted Iso is still an Iso, while an inverted Prism is just a Review
re :: (Re q s t s t -> Re p s t a b) -> p b a -> p t s
re = (`_Re` (\q -> q))
_re :: (Re q s t s t -> Re p s t a a) -> p a a -> p t s
_re = (`_Re` (\q -> q))

-- TODO: should it use Loop', or some 'EmptyP' class?
-- | Turn around a
--
-- Warning: If the loop operation on @p@ does not converge, the re'd optic will not terminate
instance Loop' p => Traversed' (Re p s t) where
  _R (Re l) = Re (\p -> l (loopRight p))
instance Traversed' p => Loop' (Re p s t) where
  loopRight (Re l) = Re (\p -> l (_R p))
