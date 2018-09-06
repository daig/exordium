module X.Functor.IMap (IMap(..),module X) where
import X.Functor.Map as X
import X.Num.FromNatural
import X.Num.Eq' as X
import X.Type.I

-- | imap f . imap g = imap (\i -> f i . g i)
--   imap (\_ -> f) = map f
class Map f => IMap i f where
  {-# minimal imap #-}
  imap :: (i -> a -> b) -> f a -> f b
  adjust :: Eq' i => (a -> a) -> i -> f a -> f a
  adjust f i = imap (\i' a -> if i `eq` i' then f a else a)

instance Zero i => IMap i ((,) x) where
  imap iab (x,a) = (x,iab zero a)
  adjust f _ = map f
instance FromNatural i => IMap i [] where
  imap iab = go zero where
    go i = \case
      [] -> []
      a:as -> iab i a : go (add one i) as
instance Zero i => IMap i I where
  imap iab (I a) = I (iab zero a)
  adjust f _ = map f
