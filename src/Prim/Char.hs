{-# language MagicHash #-}
module Prim.Char
  (Char#, module X
  ,gtChar#, geChar#, eqChar#, neChar#, ltChar#, leChar#
  ,ord#) where
import GHC.Prim
import Prim.Int as X (Int#)
