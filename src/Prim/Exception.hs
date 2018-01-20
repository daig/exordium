{-# language MagicHash #-}
module Prim.Exception (module X) where
import GHC.Base as X (RealWorld)
import GHC.Prim as X
  (State#
  ,catch#, raise#, raiseIO#
  ,maskAsyncExceptions#, maskUninterruptible#
  ,unmaskAsyncExceptions#, getMaskingState#)
