{-# GHC_OPTIONS -wno-orphans #-}
module Maybe (Maybe(..),maybe, maybe'map,module X) where
import GHC.Base (Maybe(..))
import Traverse as X
import Distribute as X
import Align

instance Traverse0 Maybe where
  traverse0 afb = \case
    Nothing -> pure Nothing
    Just a -> Just `map` afb a
instance Traverse Maybe where traverse = traverse0
instance Fold Maybe where foldMap = foldMap0
instance Fold0 Maybe where
  foldMap0 m = \case
    Nothing -> zero
    Just a -> m a

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
