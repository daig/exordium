module PureT (PureT(..), module X) where
import MapT as X
import Monad as X
import Lifts as X (Lifts)
import Constraint as X

{-instance Lifts Monad II-}
{-instance Lifts Map II-}

class (Lifts (LiftC t) t, MapT t) => PureT t where
  type LiftC t :: (* -> *) -> Constraint
  type LiftC t = Map
  pureT :: c m => m --> t m

{-newtype II m a = II {runII :: m a}-}
  {-deriving newtype (MapIso,Map,Pure,Apply,Applicative)-}
{-instance Bind m => Bind (II m) where f =<< II m = II ((\x -> runII (f x)) =<< m)-}
{-instance Monad m => Monad (II m)-}
{-instance MapT II where mapT f (II m) = II (f m)-}
{-instance PureT II where-}
  {-{-type LiftC II = Monad-}-}
  {-pureT = II-}
