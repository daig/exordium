{-# language MagicHash #-}
module X.Prim.StablePtr
  (StablePtr#, StableName#, State#, RealWorld, Int#
  ,makeStablePtr#, deRefStablePtr#, eqStablePtr#
  ,makeStableName#, eqStableName#, stableNameToInt#) where
import GHC.Prim
