module APrism.Type (APrism(..),module X) where
import E as X

data APrism a b s t = APrism (s -> E t a) (b -> t)
