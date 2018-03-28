{-# language MagicHash #-}
module Prim.IO
  (State#, RealWorld, module X
  ,realWorld#
  -- * Delay/wait operations
  ,delay#,waitRead#,waitWrite#
  ,touch#
  ,par#, spark#, seq#, getSpark#, numSparks#) where
import GHC.Prim
import GHC.Prim as X (Int#)
