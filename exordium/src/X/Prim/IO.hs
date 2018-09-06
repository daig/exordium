{-# language MagicHash #-}
{-# language UnboxedTuples #-}
module X.Prim.IO
  (State#, RealWorld, IO#
  ,IOInt#, IOWord#, IOFloat#, IODouble#, IOAddr#, IOCompact#
  ,realWorld#
  -- * Delay/wait operations
  ,delay#,waitRead#,waitWrite#
  ,touch#
  ,par#, spark#, seq#, getSpark#, numSparks#
  ,module X) where
import GHC.Prim hiding (seq#,spark#,numSparks#,par#)
import GHC.Prim as X (Int#)
import qualified GHC.Prim as GHC

type IO# a = State# RealWorld -> (# State# RealWorld, a #)
type IOInt# = State# RealWorld -> (# State# RealWorld, Int# #)
type IOWord# = State# RealWorld -> (# State# RealWorld, Word# #)
type IOFloat# = State# RealWorld -> (# State# RealWorld, Float# #)
type IODouble# = State# RealWorld -> (# State# RealWorld, Double# #)
type IOThreadId# = State# RealWorld -> (# State# RealWorld, ThreadId# #)
type IOAddr# = State# RealWorld -> (# State# RealWorld, Addr# #)
type IOCompact# = State# RealWorld -> (# State# RealWorld, Compact# #)

spark# :: a -> IO# a
{-# INLINE spark# #-}
spark# = GHC.spark#

seq# :: a -> IO# a
{-# INLINE seq# #-}
seq# = GHC.seq#

numSparks# :: IOInt#
{-# INLINE numSparks# #-}
numSparks# = GHC.numSparks#

par# :: a -> Int#
{-# DEPRECATED par# "use spark# instead: https://ghc.haskell.org/trac/ghc/ticket/15227#comment:2" #-}
{-# INLINE par# #-}
par# = GHC.par#
