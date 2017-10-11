module I (I(..), IsI, module X) where
import Map as X
import LinFoldMap as X
import Applicative as X
import Distributive as X
{-import Traverse-}
import GHC.Show (Show)

-- | Identity type
newtype I a = I a deriving Show
instance Map I where map f (I a) = I (f a)
instance LinFoldMap I where foldMap_ f (I a) = f a
instance RelFoldMap I where foldMap1 = foldMap_
instance AffFoldMap I where foldMap0 = foldMap_
instance FoldMap I where foldMap = foldMap_
instance Apply I where I f |@| I a = I (f a)
instance Pure I where pure = I
instance Distributive I where distribute a = I (map fold_ a)

type IsI f = (LinFoldMap f, Applicative f, Distributive f)
