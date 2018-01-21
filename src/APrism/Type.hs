module APrism.Type (APrism(..), module X) where
import E.Type as X

data APrism a b s t = APrism (s -> E t a) (b -> t)
