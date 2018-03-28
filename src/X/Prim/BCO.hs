{-# language MagicHash #-}
-- | Support for the bytecode interpreter and linker.
module X.Prim.BCO
  (BCO#
  ,mkApUpd0#
  ,newBCO#
  ,unpackClosure#
  ,getApStackVal#
  -- * Re-exported Types
  ,module X
  ) where
import GHC.Prim
import GHC.Prim as X (State#,ByteArray#,Array#,Int#)
