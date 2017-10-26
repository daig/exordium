module Forget where
import Zero as X
import Strong as X
import Map as X
import Traversed as X
import BiComap as X
import Comap as X
import K

newtype Forget r a b = Forget (a -> r)
instance Strong (Forget r) where
  first (Forget z) = Forget (\(a,_) -> z a)
instance Dimap (Forget r) where
  dimap f _ (Forget z) = Forget (premap f z)
instance Map (Forget r a) where map _ (Forget z) = Forget z
instance BiComap (Forget r) where
  bicomap f _ (Forget z) = Forget (premap f z)
instance Comap (Forget r a) where comap _ (Forget z) = Forget z

instance Def r => Choice (Forget r) where
  left (Forget z) = Forget (either z (\_ -> def))

instance Zero r => Traversed (Forget r) where
  wander l (Forget ar) = Forget (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Def r => AffTraversed (Forget r) where
  wander0 l (Forget ar) = Forget (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Plus r => RelTraversed (Forget r) where
  wander1 l (Forget ar) = Forget (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance LinTraversed (Forget r) where
  wander_ l (Forget ar) = Forget (\s -> case (l (\a -> K (ar a))) s of {K r -> r})