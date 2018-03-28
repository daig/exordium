module X.Arrow.Representable (Representable(..), module X) where
import X.Arrow.Sieve as X
import X.Arrow.Tabulated as X
import X.Type.I

class (Sieve p, Tabulated p, Traversed_ p) => Representable p where
  {-tabulatedP :: Iso' (a -> Rep p b) (p a b)-}

{-traversal_Default :: Representable p => (forall f. Map f => (a -> f b) -> s -> f t) -> p a b -> p s t-}
{-traversal_Default l p = tabulateP (l (sieve p))-}

{-firstDefault :: Representable p => p a b -> p (a,y) (b,y)-}
{-firstDefault p = tabulateP (\(a,y) -> map (\b -> (b,y)) (sieve p a))-}

{-leftDefault :: Representable p => p a b -> p (E a y) (E b y)-}
{-leftDefault p = tabulateP f where-}
 {-f = \case-}
   {-L a -> map L (seive p a)-}
   {-R y -> -- needs pure-}

{-secondDefault :: Representable p => p a b -> p (x,a) (x,b)-}
{-secondDefault p = tabulateP (\(x,a) -> map (x,) (sieve p a))-}

instance Tabulated (->) where tabulateP f a = case f a of I b -> b
instance Representable (->)
