module Prisms
  (module Prisms
  ,module X) where
import Prisms.APrism as X (APrism,APrism')
import Prisms.APrism
import Option as X
import K as X (KK)
import K
import Prism as X

type (s ~+. a) b t = APrism a b a b -> APrism a b s t
withPrism :: (s ~+. a) b t -> ((s -> E t a) -> (b -> t) -> r) -> r
withPrism l k = case l (APrism R (\x -> x)) of APrism h g -> k h g

prism' :: (s -> (?) a) -> (b -> s) -> (s ~+ a) b s
prism' sma bs = prism (\s -> case sma s of {Some a -> R a; None -> L s}) bs

review :: (KK b b -> KK t t) -> b -> t
review l = \b -> case l (KK b) of KK t -> t
