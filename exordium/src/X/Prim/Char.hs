{-# language MagicHash #-}
-- | Unboxed 31-bit characters.
module X.Prim.Char
  (Char#, module X
  ,gtChar#, geChar#, eqChar#, neChar#, ltChar#, leChar#
  ,ord#) where
import GHC.Prim
import X.Prim.Int as X (Int#)
