{-# language MagicHash #-}
module Prim.MVar
  (MVar#, State#, Int#
  ,newMVar#
  ,takeMVar#, tryTakeMVar#
  ,putMVar#, tryPutMVar#
  ,readMVar#, tryReadMVar#
  ,sameMVar#, isEmptyMVar#) where
import GHC.Prim
