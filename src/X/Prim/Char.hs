{-# language MagicHash #-}
module X.Prim.Char
  (Char#, module X
  ,gtChar#, geChar#, eqChar#, neChar#, ltChar#, leChar#
  ,ord#) where
import GHC.Prim
import X.Prim.Int as X (Int#)
