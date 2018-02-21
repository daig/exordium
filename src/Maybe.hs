module Maybe (Maybe(..),maybe'map,module X) where
import GHC.Base
import Map.Class as X

instance MapIso Maybe where mapIso _ = maybe'map
instance Map Maybe where map f = \case {Nothing -> Nothing; Just a -> Just (f a)}
maybe'map :: (a -> b) -> Maybe a -> Maybe b
maybe'map f = \case
  Nothing -> Nothing
  Just a -> Just (f a)

