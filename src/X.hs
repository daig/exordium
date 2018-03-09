module X (X, __, absurd) where
import qualified Prelude as P

data X 

__ :: a
__ = P.undefined

absurd :: X -> a
absurd = \case {}
