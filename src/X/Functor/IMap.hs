module X.Functor.IMap where
import X.Num.FromNatural
import X.Type.I

-- | imap f . imap g = imap (\i -> f i . g i)
--   imap (\_ a -> a) = id
class IMap i f where imap :: (i -> a -> b) -> f a -> f b

instance Zero i => IMap i ((,) x) where imap iab (x,a) = (x,iab zero a)
instance FromNatural i => IMap i [] where
  imap iab = go zero where
    go i = \case
      [] -> []
      a:as -> iab i a : go (add one i) as
instance Zero i => IMap i I where imap iab (I a) = I (iab zero a)
