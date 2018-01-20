module Type.APrism (APrism(..), module X) where
import Type.E as X

data APrism a b s t = APrism (s -> E t a) (b -> t)
