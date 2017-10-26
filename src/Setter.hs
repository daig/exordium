module Setter
  (type (%~), type (%~~)) where
  {-,setter --, cloneSetter-}
  {-,module X) where-}

type (s %~ a) b t = (a -> b) -> s -> t
type s %~~ a = (a -> a) -> s -> s
