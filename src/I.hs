module I (I(..), module X) where
import Map as X
import LinFoldMap as X
import Applicative as X
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
instance Applicative I

