module Comonad.Class (module Comonad.Class, module X) where
import Coapplicative.Class as X
import Duplicate.Class as X

-- | extend extract = id
-- extract < extend f = f
class (Coapplicative w,Duplicate w) => Comonad w
