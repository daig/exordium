module Option (type (?)(..), module X) where
import Pure as X

data (?) a = None | Some a

instance Map (?) where map f = \case {None -> None; Some a -> Some (f a)}
instance Pure (?) where pure = Some
