module X.Transformer.Unlift where
import X.Functor.Monad

class (Monad m, Monad n) => MonadLift m n | m -> n where liftBase :: b --> m
class (MonadTransControl t, forall a. StT t a ~ a) => MonadTransUnlift t
class (MonadBaseControl b m , forall a. StM m a ~ a) => MonadBaseUnlift b m


((forall x. m x -> b (StM m x)) -> m b -> b a) -> m a
StM m a -> m a

