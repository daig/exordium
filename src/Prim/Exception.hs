{-# language MagicHash #-}
module Prim.Exception
  (State#, RealWorld
  ,catch#, raise#, raiseIO#
  ,maskAsyncExceptions#, maskUninterruptible#
  ,unmaskAsyncExceptions#, getMaskingState#) where
import GHC.Prim
