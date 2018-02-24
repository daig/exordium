{-# language MagicHash #-}
module Distribute (module Distribute, module X) where
import Distribute.Class as X
import Distribute.Internal
import Map
import {-# source #-} I
import {-# source #-} K
import Coerce (mapCoerce#)

-- TODO: merge into data family
zip' :: Distribute t => (a -> a -> b) -> t a -> t a -> t b
zip' f t t' = zipF (\(V2 a b) -> f a b) (V2 t t')

-- TODO: Avoid incomplete pattern
zipF_zip :: Distribute t => (a -> b -> r) -> t a -> t b -> t r
zipF_zip f t t' = zipF (\(V2 (L a) (R b)) -> f a b) (V2 (map L t) (map R t'))

zipF_ap :: Distribute t => t (a -> r) -> t a -> t r
zipF_ap = zipF_zip (\f -> f)

collect_map :: Distribute t => (a -> b) -> t a -> t b
collect_map f ta = case collect (\x -> I (f x)) ta of I tb -> tb

distribute_distR :: Distribute t => E x (t a) -> t (E x a)
distribute_distR = distribute

distribute_pure :: forall t a. Distribute t => a -> t a
distribute_pure a = mapCoerce# @a (distribute (K a))
