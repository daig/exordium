module X.Num.Diff (Diff(..), module X) where
import X.Num.Act as X

--  | act (diff x y) x = y
class Act a x => Diff a x where diff :: x -> x -> a
