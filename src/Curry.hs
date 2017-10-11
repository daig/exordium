module Curry (Curry(..), Curry'(..), module X) where
import Map as X

newtype Curry f a b = Curry {getCurry :: f (a,b)}
newtype Curry' f b a = Curry' {getCurry' :: f (a,b)}

instance Map f => Map (Curry f a) where map f (Curry fab) = Curry (map (\(x,b) -> (x,f b)) fab)
instance Map f => Map (Curry' f b) where map f (Curry' fba) = Curry' (map (\(a,x) -> (f a,x)) fba)
