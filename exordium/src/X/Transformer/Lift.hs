module X.Transformer.Lift (Lift(..),module X) where
import X.Transformer.Map as X
import X.Functor.Monad as X
import X.Kind.Type
import X.Kind.Constraint

{-instance Lifts Monad II-}
{-instance Lifts Map II-}

class (forall m. c m => c' (t m))
   => Lift (c  :: (Type -> Type) -> Constraint)
           (c' :: (Type -> Type) -> Constraint)
           (t  :: (Type -> Type) -> Type -> Type)
  where
    -- | Hello
    lift :: c m => m a -> t m a

{-newtype II m a = II {runII :: m a}-}
  {-deriving newtype (MapIso,Map,Pure,Apply,Applicative)-}
{-instance Bind m => Bind (II m) where f =<< II m = II ((\x -> runII (f x)) =<< m)-}
{-instance Monad m => Monad (II m)-}
{-instance TMap II where mapT f (II m) = II (f m)-}
{-instance PureT II where-}
  {-{-type LiftC II = Monad-}-}
  {-pureT = II-}

