{-# language MagicHash #-}
module X.Num.Diff (Diff(..), module X) where
import X.Num.Act as X
import X.Prim.Addr
import X.Data.Addr
import X.Type.Int

--  | act (diff x y) x = y
class Act a x => Diff a x where diff :: x -> x -> a

instance Diff Int Addr where diff (Addr# a) (Addr# b) = I# (minusAddr# a b)
