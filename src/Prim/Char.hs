{-# language MagicHash #-}
module Prim.Char (module X) where
import Prim.Int as X (Int#)
import GHC.Prim as X
  (Char#
  ,gtChar#, geChar#, eqChar#, neChar#, ltChar#, leChar#
  ,ord#)
