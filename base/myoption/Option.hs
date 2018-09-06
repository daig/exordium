{-# language MagicHash #-}
module Option where
import Bool
data Maybe a = Nothing | Just a
nothing = Nothing
just = Just

maybe :: b -> (a -> b) -> Maybe a -> b
maybe = maybe
isJust :: Maybe a -> Bool
isJust = \case {Just{} -> 1#; _ -> 0#}
isNothing :: Maybe a -> Bool
isNothing = \case {Nothing -> 1#; _ -> 0#}
fromMaybe :: a -> Maybe a -> a
fromMaybe = fromMaybe
listToMaybe :: [a] -> Maybe a
listToMaybe = listToMaybe
maybeToList :: Maybe a -> [a]
maybeToList = maybeToList
catMaybes :: [Maybe a] -> [a]
catMaybes = catMaybes
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe = mapMaybe
