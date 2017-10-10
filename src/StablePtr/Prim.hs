module StablePtr.Prim (module X) where
import GHC.Prim as X
  (StablePtr#, StableName#, State#, RealWorld, Int#
  ,makeStablePtr#, deRefStablePtr#, eqStablePtr#
  ,makeStableName#, eqStableName#, stableNameToInt#)
