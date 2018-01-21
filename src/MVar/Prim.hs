{-# language MagicHash #-}
module MVar.Prim
  (MVar#, State#, Int#
  ,newMVar#
  ,takeMVar#, tryTakeMVar#
  ,putMVar#, tryPutMVar#
  ,readMVar#, tryReadMVar#
  ,sameMVar#, isEmptyMVar#) where
import GHC.Prim
