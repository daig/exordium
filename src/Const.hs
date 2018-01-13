module Const (Const(..), module X) where

import Dimap as X

class Dimap p => Const p where const :: a -> p x a

instance Const (->) where const a _ = a
