{-# language MagicHash #-}
module X.Prim.Exception
  (catch#
  ,raise#
  ,raiseIO# -- $raiseIO
  ,maskAsyncExceptions#, maskUninterruptible#
  ,unmaskAsyncExceptions#, getMaskingState#
  -- * Re-exported Types
  ,State#, RealWorld
) where
import GHC.Prim

-- $raiseIO
-- @raiseIO#@ needs to be a primop, because exceptions in the IO monad
-- must be /precise/ - we don't want the strictness analyser turning
-- one kind of bottom into another, as it is allowed to do in pure code.
--
-- But we /do/ want to know that it returns bottom after
-- being applied to two arguments, so that this function is strict in y
--
-- >   f x y | x>0       = raiseIO blah
-- >         | y>0       = return 1
-- >         | otherwise = return 2
