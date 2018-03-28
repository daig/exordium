{-# language MagicHash #-}
module X.Data.Struct.Natural (Natural(..), GmpLimb#, module X) where
import X.Data.Struct.BigNat as X
import GHC.Integer.GMP.Internals (GmpLimb#)
import GHC.Natural
