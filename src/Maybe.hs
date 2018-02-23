module Maybe (Maybe(..),maybe, maybe'map,module X) where
import GHC.Base (Maybe(..))
import Traverse0 as X
import Map.Class as X
import Distributive as X
import Align.Class

instance Traverse0 Maybe where
  traverse0 afb = \case
    Nothing -> pure Nothing
    Just a -> Just `map` afb a
instance Traverse Maybe where traverse = traverse0
instance FoldMap Maybe where foldMap = foldMap0
instance FoldMap0 Maybe where
  foldMap0 m = \case
    Nothing -> zero
    Just a -> m a

instance MapIso Maybe where mapIso _ = maybe'map
instance Map Maybe where map f = \case {Nothing -> Nothing; Just a -> Just (f a)}
maybe'map :: (a -> b) -> Maybe a -> Maybe b
maybe'map f = \case
  Nothing -> Nothing
  Just a -> Just (f a)

instance Pure Maybe where pure = Just

instance Align Maybe where
  alignWith ar br abr = go where
    go Nothing Nothing = Nothing
    go Nothing (Just b) = Just (br b)
    go (Just a) Nothing = Just (ar a)
    go (Just a) (Just b) = Just (abr a b)

maybe :: r -> (a -> r) -> Maybe a -> r
maybe z f = \case
  Nothing -> z
  Just x -> f x
