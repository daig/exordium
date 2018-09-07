module Ord (Eq#,Ord#,gt#,ge#,lt#,le#,eq#,ne#) where
import GHC.Classes
import Bool
import Eq

type Ord# = Ord

gt# :: Ord# a => a -> a -> Bool
{-# inline gt# #-}
gt# = (>)

ge# :: Ord# a => a -> a -> Bool
{-# inline ge# #-}
ge# = (>=)

lt# :: Ord# a => a -> a -> Bool
{-# inline lt# #-}
lt# = (<)

le# :: Ord# a => a -> a -> Bool
{-# inline le# #-}
le# = (<=)

min# :: Ord# a => a -> a -> a
{-# inline min# #-}
min# = min

max# :: Ord# a => a -> a -> a
{-# inline max# #-}
max# = max
