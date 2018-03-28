module X.Arrow.Transformed (Transformed(..), liftWithOf, module X) where
import X.Transformer.Pure as X
import X.Arrow.Promap as X
class Promap p => Transformed p where
  transformed :: (TPure t, LiftC t f, LiftC t g) => p (f x) (g x) -> p (t f x) (t g x)

liftWithOf :: (forall p. Transformed p => p (a x) (b x) -> p (s x) (t x)) -> ((s x -> a x) -> b x) -> t x
liftWithOf = liftWithOf
