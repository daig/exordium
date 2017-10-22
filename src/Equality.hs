{-# language TypeInType #-}
module Equality
  (type (==~), type (==~~)
  ,module X) where
import GHC.Types as X (type (*))
import Category as X (id)

type (a ==~  s) b t = forall p. p a b -> p s t
type  s ==~~ a      = forall p. p a a -> p s s
