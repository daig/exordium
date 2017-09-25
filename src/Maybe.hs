module Maybe where
import Map
import Pure
import Append 
import Sum

data Maybe a = Nothing | Just a

instance Map Maybe where map f = \case {Nothing -> Nothing; Just a -> Just (f a)}
instance Pure Maybe where pure = Just
instance Append Maybe where
  (|+|) Nothing = map R
  (|+|) (Just a) = \_ -> Just (L a)
  append Nothing = \x -> x
  append x = \_ -> x


