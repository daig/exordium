module Setter (type (%~), type (%~~), type (%~.), type (%~~.), module X) where
import I as X

-- | (%~) = forall f. IsI f => Optic f
type (s %~ a) b t = forall f. (IsI f) => (a -> f b) -> s -> f t
-- | (%~~) = forall f. IsI f => Optic' f
type s %~~ a = forall f. (IsI f) => (a -> f a) -> s -> f s
-- | (%~.) = Optic I
type (s %~. a) b t = (a -> I b) -> s -> I t
-- | (%~.) = Optic' I
type s %~~. a = (a -> I a) -> s -> I s

{-setter :: ((a -> b) -> s -> t) -> (s %~ a) b t-}
{-setter f afb s = -}

{-cloneSetter :: (s %~. a) b t -> (s %~ a) b t-}
{-cloneSetter l afb = postmap# pure $ l (postmap fold_-}
