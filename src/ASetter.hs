module ASetter (
  type (%~.), type (%~~.)
  ,set, over
  ) where
import I as X
import Coerce (postmap#)

type (s %~. a) b t = (a -> I b) -> s -> I t
type s %~~. a = (a -> I a) -> s -> I s

set :: (s %~. a) b t -> b -> s -> t
set l b = (\(I a) -> a) `postmap#` l (\_ -> I b)

over :: (s %~. a) b t -> (a -> b) -> s -> t
over l f = (\(I a) -> a) `postmap#` l (\a -> I (f a))
