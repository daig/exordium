module Int (module X) where

import GHC.Int as X
  (Int(..)
  ,Int8(..)
  ,Int16(..)
  ,Int32(..)
  ,Int64(..))
import PlusZero as X
import qualified Prelude as P

instance PlusZero Int
instance Plus Int where (+) = (P.+)
instance Zero Int   where zero = 0
instance PlusZero Int16
instance Plus Int8  where (+) = (P.+)
instance Zero Int8  where zero = 0
instance Plus Int16 where (+) = (P.+)
instance Zero Int16 where zero = 0
instance PlusZero Int32
instance Plus Int32 where (+) = (P.+)
instance Zero Int32 where zero = 0
instance PlusZero Int64
instance Plus Int64 where (+) = (P.+)
instance Zero Int64 where zero = 0
