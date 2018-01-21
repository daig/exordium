{-# language MagicHash #-}
module Char.Prim
  (Char#, module X
  ,gtChar#, geChar#, eqChar#, neChar#, ltChar#, leChar#
  ,ord#) where
import GHC.Prim
import Int.Prim as X (Int#)
