{-# language UnboxedTuples #-}
module X.Functor.Remap (Remap(..)) where
import X.Type.I
import X.Type.K
import X.Type.IO
import X.Data.E
import X.Cast.Coerce as X (type ( #=# ),coerce,coerceF)
import X.Data.Maybe
import X.Data.These
import Control.Arrow ((***))

class Remap f where remap :: (b -> a) -> (a -> b) -> f a -> f b

instance Remap ((->) x) where
  remap _ f p = \a -> f (p a)
instance Remap [] where
  remap _ f = go where
    go = \case
      [] -> []
      a:as -> f a : go as
instance Remap ((,) x) where remap _ f (x,y) = (x,f y)
instance Remap (K a) where remap _ _ = coerce
instance Remap I where remap _ f (I a) = I (f a)

instance Remap (E x) where
  remap _ f = \case
    L a -> L a
    R b -> R (f b)

instance Remap Maybe where remap _ = maybe'map
maybe'map :: (a -> b) -> Maybe a -> Maybe b
maybe'map f = \case
  Nothing -> Nothing
  Just a -> Just (f a)

instance Remap IO where remap _ f (IO io) = IO (\s -> case io s of (# s', a #) -> (# s', f a #))
instance Remap (These a) where
  remap _ f = \case
    This a -> This a
    That b -> That (f b)
    These a b -> These a (f b)
