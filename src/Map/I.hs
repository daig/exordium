module Map.I where
import Zero
import Prelude (Enum(..))

class IMap i f where imap :: (i -> a -> b) -> f a -> f b

instance Zero i => IMap i ((,) x) where imap iab (x,a) = (x,iab zero a)
instance Enum i => IMap i [] where
  imap iab = go (toEnum 0) where
    go i = \case
      [] -> []
      a:as -> iab i a : go (succ i) as
