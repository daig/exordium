{-# language MagicHash #-}
module X.Prim.MutVar
  (MutVar#
  ,newMutVar#, readMutVar#, writeMutVar#
  ,atomicModifyMutVar# -- $mutVar
  ,sameMutVar#
  ,casMutVar# -- | (Unsafe) atomic compare-and-swap

  -- * Re-exported Types
  ,module X
  ) where
import GHC.Prim
import GHC.Prim as X (Int#,State#)
{-TODO: document compare-and-swap - possible improvement in atomic-primops-}

-- $mutVar
-- Looking at the type of @atomicModifyMutVar#@, one might wonder why
-- it doesn't return an unboxed tuple. e.g.,
--
--   > MutVar# s a -> (a -> (# a, b #)) -> State# s -> (# State# s, b #)
--
-- The reason is that @atomicModifyMutVar#@ relies on laziness for its atomicity.
-- Given a 'MutVar#' containing x, @atomicModifyMutVar#@ merely replaces the
-- its contents with a thunk of the form @(fst (f x))@. This can be done using an
-- atomic compare-and-swap as it is merely replacing a pointer.
