module Traverse0 (module Traverse0, module X) where
import Traverse0.Class as X
{-import FoldMap1 as X-}
import Traverse as X
import K.Type
import Instances

defaulting 'foldMap0 [|\f ta -> case traverse0 (\a -> K (f a)) ta of K m -> m|]
defaulting 'traverse [|traverse0|]
