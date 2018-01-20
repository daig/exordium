module Class.Comonad (module Class.Comonad, module X) where
import Class.Coapplicative as X
import Class.Duplicate as X

-- | extend extract = id
-- extract < extend f = f
class (Coapplicative w,Duplicate w) => Comonad w
