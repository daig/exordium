module Void where
import Plus
import Times
import qualified Prelude as P

__ :: a
__ = P.undefined
data X
instance Plus X where (+) = \case {}
instance Times X where (*) = \case {}
