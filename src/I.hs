module I (I(..), module X) where
import Map as X
import Traverse_ as X
import FoldMap_ as X
import Applicative as X
import GHC.Show (Show)

-- | Identity type
newtype I a = I a deriving Show
instance MapIso I where mapIso = map_mapIso
instance Map I where map f (I a) = I (f a)
instance FoldMap_ I where foldMap_ f (I a) = f a
instance FoldMap1 I where foldMap1 = foldMap_
instance FoldMap0 I where foldMap0 = foldMap_
instance FoldMap I where foldMap = foldMap_
instance Apply I where I f |$| I a = I (f a)
instance Pure I where pure = I
instance Applicative I

instance Traverse_ I where traverse_ f (I a) = map I (f a)
instance Traverse0 I where traverse0 = traverse_
instance Traverse1 I where traverse1 = traverse_
instance Traverse I where traverse = traverse_

