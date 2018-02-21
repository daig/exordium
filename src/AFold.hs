module AFold (module AFold, module X) where
import Dimap
import Forget as X
import qualified Prelude as P
import {-# source #-} Maybe
import Pure.Class


toListOf :: (Forget ([a] -> [a]) a a -> Forget ([a] -> [a]) s s) -> s -> [a]
{-toListOf :: (s ^~.. a) ([a] -> [a]) -> s -> [a]-}
toListOf l s = foldMapOf l (:) s []

foldMapOf :: (Forget m a a -> Forget n s s) -> (a -> m) -> s -> n
{-foldMapOf :: (s ^~. a) m n -> (a -> m) -> s -> n-}
foldMapOf = re _Forget
{-foldMapOf l f = case l (Forget f) of Forget g -> g-}

foldOf :: (Forget a a a -> Forget a s s) -> s -> a
{-foldOf :: s ^~~. a -> s -> a-}
foldOf l = foldMapOf l (\a -> a)

-- | Synonym for foldOf
view :: (Forget a a a -> Forget a s s) -> s -> a
view = foldOf

view' :: Pure f => (Forget (f a) a a -> Forget n s s) -> s -> n
view' = (`foldMapOf` pure)
