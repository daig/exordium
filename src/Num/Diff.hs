module Num.Diff (Diff(..), module X) where
import Num.Act as X

--  | act (diff x y) x = y
class Act a x => Diff a x where diff :: x -> x -> a
