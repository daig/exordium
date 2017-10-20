{-# language TypeInType #-}
module Equality (type (==~), type (==~~)) where
import GHC.Types as X (type (*))

type ((s :: k1) ==~ (a :: k1)) (b :: k2) (t :: k2) = forall k3 (p :: k1 -> k3 -> *) (f :: k2 -> k3).
  p a (f b) -> p s (f t)
type (s :: k) ==~~ (a :: k) = forall k' (p :: k -> k' -> *) (f :: k -> k').  p a (f a) -> p s (f s)
