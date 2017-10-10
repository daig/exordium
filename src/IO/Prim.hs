module IO.Prim (module X) where
import GHC.Prim as X
  (State#, RealWorld, ThreadId#, Int#
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
  ,par#, spark#, seq#, getSpark#, numSparks#)
