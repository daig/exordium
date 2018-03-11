module Assoc where
import Map.Pro as X
import Optic.Iso
--
class Assoc f where
  {-# minimal assocl, assocr | assoced #-}
  assocl :: f a (f b c) -> f (f a b) c
  assocl = view assoced
  assocr :: f (f a b) c -> f a (f b c)
  assocr = review assoced
  assoced :: Promap p => p (f (f a b) c) (f (f a' b') c') -> p (f a (f b c)) (f a' (f b' c'))
  assoced = promap assocl assocr
instance Assoc (,) where
  assocl (a,(b,c)) = ((a,b),c)
  assocr ((a,b),c) = (a,(b,c))
instance Assoc E where
  assocl = \case
    L a -> L (L a)
    R (L b) -> L (R b)
    R (R c) -> R c
  assocr = \case
    L (L a) -> L a
    L (R b) -> R (L b)
    R c -> R (R c)
