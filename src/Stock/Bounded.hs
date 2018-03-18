{-# language MagicHash #-}
module Stock.Bounded (Bounded#,minBound#,maxBound#) where
import Stock

type Bounded# = Bounded
minBound#,maxBound# :: Bounded# a => a
minBound# = minBound
maxBound# = maxBound
