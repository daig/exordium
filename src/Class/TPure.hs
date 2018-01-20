module Class.TPure (module Class.TPure, module X) where
import Class.TMap as X
import Class.Monad as X
import Class.Lifts as X
import Type.Constraint as X

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
