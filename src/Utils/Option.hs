module Utils.Option (module Utils.Option, module X) where
import Type.Option as X

option'map :: (a -> b) -> (?) a -> (?) b
option'map f = \case
  None -> None
  Some a -> Some (f a)
