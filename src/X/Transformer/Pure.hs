module X.Transformer.Pure (TPure(..), module X) where
import X.Transformer.Map as X
import X.Functor.Monad as X
import X.Constraint.Lifts
import X.Kind.Type

{-instance Lifts Monad II-}
{-instance Lifts Map II-}

class (Lifts (LiftC t) t, TMap t) => TPure t where
  type LiftC t :: (Type -> Type) -> Constraint
  type LiftC t = Map
  tpure :: c m => m --> t m

{-newtype II m a = II {runII :: m a}-}
  {-deriving newtype (MapIso,Map,Pure,Apply,Applicative)-}
{-instance Bind m => Bind (II m) where f =<< II m = II ((\x -> runII (f x)) =<< m)-}
{-instance Monad m => Monad (II m)-}
{-instance TMap II where mapT f (II m) = II (f m)-}
{-instance PureT II where-}
  {-{-type LiftC II = Monad-}-}
  {-pureT = II-}
