{-# language MagicHash #-}
module Prim.MVar where
import GHC.Prim as X
  (MVar#, State#, Int#
  ,newMVar#
  ,takeMVar#, tryTakeMVar#
  ,putMVar#, tryPutMVar#
  ,readMVar#, tryReadMVar#
  ,sameMVar#, isEmptyMVar#)
