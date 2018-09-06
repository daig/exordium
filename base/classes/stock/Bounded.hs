{-# language MagicHash #-}
module Bounded (Bounded#,minBound#,maxBound#) where
import GHC.Enum

type Bounded# = Bounded
minBound#,maxBound# :: Bounded# a => a
minBound# = minBound
maxBound# = maxBound
