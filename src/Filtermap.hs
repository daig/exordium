module Filtermap where
import Class.Map
import Bool
import Type.Option

class Map f => Filtermap f where
  {-# minimal filtermap | filter #-}
  filtermap :: (a -> (?) b) -> f a -> f b
  filtermap f x = map (\case Some a -> a)
                ( filter (\case {None -> False; _ -> True})
                ( map f x))
  filter :: (a -> Bool) -> f a -> f a
  filter f x =  filtermap (\a -> case f a of {False -> None; True -> Some a}) x

filtermap_map :: Filtermap f => (a -> b) -> f a -> f b
filtermap_map f = filtermap (\a -> Some (f a))
