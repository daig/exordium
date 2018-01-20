module Utils.Maybe (module Utils.Maybe, module X) where
import Type.Maybe as X

maybe'map :: (a -> b) -> Maybe a -> Maybe b
maybe'map f = \case
  Nothing -> Nothing
  Just a -> Just (f a)
