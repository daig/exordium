{-# language MagicHash #-}
module Struct.Natural (Natural(..), GmpLimb#, module X) where
import Struct.BigNat as X
import GHC.Integer.GMP.Internals (GmpLimb#)
import GHC.Natural
