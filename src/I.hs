module I where
import Map
import Extract
import Apply
import Pure
import Distributive
import Traversable
import GHC.Show (Show)

-- | Identity type
newtype I a = I a deriving Show
instance Map I where map f (I a) = I (f a)
instance Extract I where extract (I a) = a
instance Apply I where I f |@| I a = I (f a)
instance Pure I where pure = I
instance Distributive I where
  distribute a = I (map extract a)
-- | Deferred lazy data
data D a = D ~a
instance Map D where map f (D a) = D (f a)
instance Extract D where extract (D a) = a
instance Apply D where D f |@| D a = D (f a)
instance Pure D where pure = D
instance Distributive D where
  distribute a = D (map extract a)

type IsI f = (Traversable f, Distributive f, Apply f, Pure f)
