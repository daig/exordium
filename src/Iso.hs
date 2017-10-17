{-# language UndecidableInstances #-}
module Iso where
import Exchange as X
import I as X

type (s =~ a) b t = forall p f. (Dimap p, Map f) => p a (f b) -> p s (f t)
type (s =~~ a) = forall p f. (Dimap p, Map f) => p a (f a) -> p s (f s)
type (s =~. a) b t = Exchange a b a (I b) -> Exchange a b s (I t)
type (s =~~. a) b t = Exchange a a a (I a) -> Exchange a a s (I s)

iso :: (s -> a) -> (b -> t) -> (s =~ a) b t
iso sa bt = dimap sa (map bt)
{-# inline iso #-}


class f ~=: g where natIsoed :: (f a =~ g a) (g b) (f b)
class (s ~= a) b t where isoed :: (s =~ a) b t
instance f ~=: g => (f a ~= g a) (g b) (f b) where isoed = natIsoed
