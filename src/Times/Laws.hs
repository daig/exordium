module Times.Laws where
import Times
import Bool

assoc :: (Eq a, Times a) => a -> a -> a -> Bool
assoc a b c = (a*b)*c == a*(b*c)
--TODO: add more
