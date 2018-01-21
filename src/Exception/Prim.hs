{-# language MagicHash #-}
module Exception.Prim
  (State#, RealWorld
  ,catch#, raise#, raiseIO#
  ,maskAsyncExceptions#, maskUninterruptible#
  ,unmaskAsyncExceptions#, getMaskingState#) where
import GHC.Prim
