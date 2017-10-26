module Void where
import Plus
import Times
import Power
import qualified Prelude as P

__ :: a
__ = P.undefined
data X
instance Plus X where (+) = \case {}
instance Times X where (*) = \case {}
instance Power a X where (^) _ = \x -> x
instance Power X a where (^) = \case {}
