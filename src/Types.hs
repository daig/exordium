module Types (TYPE,module X) where

import GHC.Int as X
  (Int(..)
  ,Int8(..)
  ,Int16(..)
  ,Int32(..)
  ,Int64(..))
import GHC.Word as X
  (Word(..)
  ,Word8(..)
  ,Word16(..)
  ,Word32(..)
  ,Word64(..))
import GHC.Num as X (Integer(..))
import GHC.Types as X
  (Bool(..)
  ,Char(..)
  ,Constraint
  ,Double(..)
  ,Float(..)
  ,IO(..)
  ,Nat
  ,Ordering
  ,RuntimeRep(..)
  ,Type
  ,TYPE(..)
  ,VecCount(..)
  ,VecElem(..)
  )
import GHC.Natural as X (Natural(..))
import GHC.Real as X (Ratio(..),Rational)
