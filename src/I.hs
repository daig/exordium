module I (I(..), IsI, module X) where
import Map as X
import LinFoldable as X
import Applicative as X
import Distributive as X
{-import Traversable-}
import GHC.Show (Show)

-- | Identity type
newtype I a = I a deriving Show
instance Map I where map f (I a) = I (f a)
instance LinFoldable I where foldMap_ f (I a) = f a
instance RelFoldable I where foldMap1 = foldMap_
instance AffFoldable I where foldMap0 = foldMap_
instance Foldable I where foldMap = foldMap_
instance Apply I where I f |@| I a = I (f a)
instance Pure I where pure = I
instance Distributive I where distribute a = I (map fold_ a)

type IsI f = (LinFoldable f, Applicative f, Distributive f)
