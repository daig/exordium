module Maybe (module Maybe, module X) where
import Maybe.Type as X

maybe'map :: (a -> b) -> Maybe a -> Maybe b
maybe'map f = \case
  Nothing -> Nothing
  Just a -> Just (f a)
