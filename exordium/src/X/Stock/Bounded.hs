{-# language MagicHash #-}
module X.Stock.Bounded (Bounded#,minBound#,maxBound#) where
import X.Stock

type Bounded# = Bounded
minBound#,maxBound# :: Bounded# a => a
minBound# = minBound
maxBound# = maxBound
