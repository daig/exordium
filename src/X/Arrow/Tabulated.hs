module X.Arrow.Tabulated (Tabulated(..), module X) where
import X.Rep.Type as X
import X.Arrow.Traversed as X
import X.Cast.Coerce
import X.Arrow.Sieve as X
import X.Type.I

class (Traversed_ p, Sieve p) => Tabulated p where
  tabulateP :: (a -> Rep p b) -> p a b

tabulateP__1 :: Tabulated p => p a b -> p (a,y) (b,y)
tabulateP__1 p = tabulateP (\(a,y) -> map (\b -> (b,y)) (sieve p a))
tabulateP__2 :: Tabulated p => p a b -> p (x,a) (x,b)
tabulateP__2 p = tabulateP (\(x,a) -> map (\b -> (x,b)) (sieve p a))
tabulateP_lens :: Tabulated p => (s -> a) -> (s -> b -> t) -> p a b -> p s t
tabulateP_lens sa sbt pab = tabulateP (\s -> sbt s `map` sieve pab (sa s))

tabulateP_pure_left :: (Pure (Rep p), Tabulated p) => p a b -> p (E a y) (E b y)
tabulateP_pure_left p = tabulateP f
  where f = \case L a -> map L (sieve p a)
                  R y -> pure (R y) -- needs pure

instance Tabulated (->) where tabulateP = coerce
