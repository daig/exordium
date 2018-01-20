module Utils.X (module Utils.X, module X) where
import Type.X as X
import qualified Prelude as P

__ :: a
__ = P.undefined

absurd :: X -> a
absurd = \case {}
