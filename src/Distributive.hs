{-# language MagicHash #-}
module Distributive (module Distributive, module X) where
import Distributive.Class as X
import Internal.Distributive
import Map
import I.Type
import K.Type

-- TODO: merge into data family
zip' :: Distributive t => (a -> a -> b) -> t a -> t a -> t b
zip' f t t' = zipF (\(V2 a b) -> f a b) (V2 t t')

-- TODO: Avoid incomplete pattern
zipF_zip :: Distributive t => (a -> b -> r) -> t a -> t b -> t r
zipF_zip f t t' = zipF (\(V2 (L a) (R b)) -> f a b) (V2 (map L t) (map R t'))

zipF_ap :: Distributive t => t (a -> r) -> t a -> t r
zipF_ap t t' = zipF (\(V2 (L f) (R a)) -> f a) (V2 (map L t) (map R t'))

collect_map :: Distributive t => (a -> b) -> t a -> t b
collect_map f ta = case collect (\x -> I (f x)) ta of I tb -> tb

distribute_distR :: Distributive t => E x (t a) -> t (E x a)
distribute_distR = distribute

distribute_pure :: forall t a. Distributive t => a -> t a
distribute_pure a = map# @a (distribute (K a))
