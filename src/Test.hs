module Test where

import Traverse_
import K.Type

ff :: Traverse_ t => (forall f. Pure f => a -> f a) -> t a -> K () (t a)
ff = traverse_
