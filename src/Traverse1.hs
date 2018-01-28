module Traverse1 (module Traverse1, module X) where
import Traverse1.Class as X
{-import FoldMap1 as X-}
import Traverse as X
import K.Type
import Instances

defaulting 'foldMap1 [|\f ta -> case traverse1 (\a -> K (f a)) ta of K m -> m|]
defaulting 'traverse [|traverse1|]
