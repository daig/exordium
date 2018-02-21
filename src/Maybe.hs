module Maybe (Maybe(..),maybe'map) where
import GHC.Base

maybe'map :: (a -> b) -> Maybe a -> Maybe b
maybe'map f = \case
  Nothing -> Nothing
  Just a -> Just (f a)
