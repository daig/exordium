{-# language MagicHash #-}
module Prim.IO.Weak 
  (Weak#
  ,mkWeak#, mkWeakNoFinalizer#
  ,addCFinalizerToWeak#
  ,deRefWeak#
  ,finalizeWeak#
  ) where

import GHC.Prim
