module Char.Prim (module X) where
import Int.Prim as X (Int#)
import GHC.Prim as X
  (Char#
  ,gtChar#, geChar#, eqChar#, neChar#, ltChar#, leChar#
  ,ord#)
