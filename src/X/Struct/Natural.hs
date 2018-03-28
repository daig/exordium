{-# language MagicHash #-}
module X.Struct.Natural (Natural(..), GmpLimb#, module X) where
import X.Struct.BigNat as X
import GHC.Integer.GMP.Internals (GmpLimb#)
import GHC.Natural
