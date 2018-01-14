{-# language MagicHash #-}
module Distributive
  (module Distributive
  ,module X) where
import Map as X
import I
import K
import Coerce
import Prelude (($)) -- TODO: reexport

class Applicative t => Distributive t where
  {-# minimal distribute | collect | zipF #-}
  distribute :: Map f => f (t a) -> t (f a)
  distribute = collect (\x -> x)
  -- aka cotraverse
  zipF :: Map f => (f a -> b) -> f (t a) -> t b
  zipF f = \fta -> map f (distribute fta)
  collect :: Map f => (a -> t b) -> f a -> t (f b)
  collect f a  = zipF (\x -> x) (map f a)
  -- TODO: is collect (\x -> x) === cotraverse (\x -> x)

-- TODO: merge into data family
data V2 a = V2 {v2a :: ~a, v2b :: ~a} 
instance Map V2 where map f (V2 a b) = V2 (f a) (f b)
instance Pure V2 where pure a = V2 a a
instance Distributive V2 where distribute fta = V2 (map v2a fta) (map v2b fta)
instance Apply V2 where (|$|) = zipF_ap
instance Applicative V2
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

distribute_pure :: Distributive t => a -> t a
distribute_pure a = map# (\(K x) -> x) (distribute (K a))


instance Distributive ((->) x) where
  collect axb fa = \x -> (\a -> axb a x) $@ fa
  distribute fxa = \x -> ($ x) $@ fxa

instance Distributive I where distribute a = I (map fold_ a)
