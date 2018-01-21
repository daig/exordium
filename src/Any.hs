module Any (Any, __, absurd) where
import GHC.Exts
import qualified Prelude as P

__ :: a
__ = P.undefined

absurd :: Any -> a
absurd = \case {}
