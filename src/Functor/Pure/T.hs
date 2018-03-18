module Functor.Pure.T (TPure(..), module X) where
import Transformer.Map as X
import Functor.Monad as X
import Constraint.Lifts

{-instance Lifts Monad II-}
{-instance Lifts Map II-}

class (Lifts (LiftC t) t, TMap t) => TPure t where
  type LiftC t :: (* -> *) -> Constraint
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
