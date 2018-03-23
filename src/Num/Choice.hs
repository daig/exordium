module Num.Choice (Choice(..),module X) where
import ADT.E as X

class Choice a where choice :: a -> E a a
