module AFold
  (type (^~.), type (^~..), type (^~~.)
  ,toListOf, foldMapOf, foldOf
  ,module X) where
import K as X
import qualified Prelude as P

type (s ^~. a) m n = (a -> K m a) -> s -> K n s
type (s ^~.. a) m = (a -> K m a) -> s -> K m s
type s ^~~. a = (a -> K a a) -> s -> K a s

toListOf :: (s ^~. a) ([a] -> [a]) ([a] -> [a]) -> s -> [a]
toListOf l s = case l (\a -> K (\as -> [a] P.++ as)) s of {K f -> f []}

foldMapOf :: (s ^~. a) m n -> (a -> m) -> s -> n
foldMapOf l f s = case l (\a -> K (f a)) s of K n -> n

foldOf :: s ^~~. a -> s -> a
foldOf l = foldMapOf l (\a -> a)
