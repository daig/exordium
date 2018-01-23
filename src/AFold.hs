module AFold (module AFold, module X) where
import Forget as X
import qualified Prelude as P

toListOf :: (Forget ([a] -> [a]) a a -> Forget ([a] -> [a]) s s) -> s -> [a]
{-toListOf :: (s ^~.. a) ([a] -> [a]) -> s -> [a]-}
toListOf l s = case l (Forget (:)) of Forget z -> z s []

foldMapOf :: (Forget m a a -> Forget n s s) -> (a -> m) -> s -> n
{-foldMapOf :: (s ^~. a) m n -> (a -> m) -> s -> n-}
foldMapOf l f s = case l (Forget (\a -> f a)) of Forget g -> g s

foldOf :: (Forget a a a -> Forget a s s) -> s -> a
{-foldOf :: s ^~~. a -> s -> a-}
foldOf l = foldMapOf l (\a -> a)

-- | Synonym for foldOf
view :: (Forget a a a -> Forget a s s) -> s -> a
view = foldOf
