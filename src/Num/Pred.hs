module Num.Pred (Pred(..), module X) where
import ADT.Maybe as X

class Pred s where pred :: s -> Maybe s
