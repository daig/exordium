module Utils.Monad (module Utils.Monad, module X) where
import Class.Monad as X

{-(>>) :: Monad m => m a -> m b -> m b-}
{-m >> m' = m >>= (\_ -> m')-}

{-g <=< f = \x -> g =<< f x-}
