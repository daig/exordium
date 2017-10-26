module AFold
  (type (^~.), type (^~..), type (^~~.)
  ,toListOf, foldMapOf, foldOf, view
  ,module X) where
import Forget as X
import qualified Prelude as P

type (s ^~. a) m n = Forget m a a -> Forget n s s
type (s ^~.. a) m = Forget m a a -> Forget m s s
type s ^~~. a = Forget a a a -> Forget a s s

toListOf :: (s ^~. a) ([a] -> [a]) ([a] -> [a]) -> s -> [a]
toListOf l s = case l (Forget (:)) of Forget z -> z s []

foldMapOf :: (s ^~. a) m n -> (a -> m) -> s -> n
foldMapOf l f s = case l (Forget (\a -> f a)) of Forget g -> g s

foldOf :: s ^~~. a -> s -> a
foldOf l = foldMapOf l (\a -> a)

-- | Synonym for foldOf
view :: s ^~~. a -> s -> a
view = foldOf
