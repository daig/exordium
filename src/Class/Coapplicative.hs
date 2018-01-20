module Class.Coapplicative (module Class.Coapplicative, module X) where

import Class.FoldMap_ as X
import Class.Apply as X

-- | fold_ (f |$| a) = fold_ f (fold_ a)
class (FoldMap_ f, Apply f) => Coapplicative f
