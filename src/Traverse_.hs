module Traverse_ (module Traverse_, module X) where
import Traverse_.Class as X
{-import FoldMap1 as X-}
import Traverse0 as X
import Traverse1 as X
import K.Type
import Instances

import Language.Haskell.TH (runIO)
import Prelude ((>>),print)

runIO (print "Traverse_") >> syncTH

defaulting 'foldMap_ [|\f ta -> case traverse_ (\a -> K (f a)) ta of K m -> m|]
defaulting 'traverse0 [|traverse_|]
defaulting 'traverse1 [|traverse_|]
defaulting 'traverse [|traverse_|]
