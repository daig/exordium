{-# language MagicHash #-}
module X.Prim.Bool
  (Bool#,pattern T#,pattern F#
  ) where
import GHC.Prim

type Bool# = Int#

pattern T# :: Bool#
pattern T# = 1#
pattern F# :: Bool#
pattern F# = 0#
