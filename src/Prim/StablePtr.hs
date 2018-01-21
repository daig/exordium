{-# language MagicHash #-}
module Prim.StablePtr
  (StablePtr#, StableName#, State#, RealWorld, Int#
  ,makeStablePtr#, deRefStablePtr#, eqStablePtr#
  ,makeStableName#, eqStableName#, stableNameToInt#) where
import GHC.Prim
