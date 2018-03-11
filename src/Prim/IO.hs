{-# language MagicHash #-}
module Prim.IO
  (State#, RealWorld, ThreadId#, module X
  ,fork#, forkOn#
  ,killThread#
  ,yield#
  ,myThreadId#
  ,labelThread#
  ,isCurrentThreadBound#
  ,noDuplicate#
  ,threadStatus#
  ,delay#,waitRead#,waitWrite#
  ,touch#
  ,par#, spark#, seq#, getSpark#, numSparks#) where
import GHC.Prim
import GHC.Prim as X (Int#)
