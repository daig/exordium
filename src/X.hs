module X (module X, module X) where
import X.Type as X
import qualified Prelude as P

__ :: a
__ = P.undefined

absurd :: X -> a
absurd = \case {}
