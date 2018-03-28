{-# language MagicHash #-}
module Prim.IO.MVar
  (MVar#, State#, Int#
  ,newMVar#
  ,takeMVar#, tryTakeMVar#
  ,putMVar#, tryPutMVar#
  ,readMVar#, tryReadMVar#
  ,sameMVar#, isEmptyMVar#) where
import GHC.Prim
