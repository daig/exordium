module AFold (module AFold, module X) where
import Coerce
import Dimap
import Forget as X
import qualified Prelude as P
import Maybe
import Pure.Class
import Zero.Class


toListOf :: (Forget ([a] -> [a]) a a -> Forget ([a] -> [a]) s s) -> s -> [a]
{-toListOf :: (s ^~.. a) ([a] -> [a]) -> s -> [a]-}
toListOf l s = foldMapOf l (:) s []

foldMapOf :: (Forget m a a -> Forget n s s) -> (a -> m) -> s -> n
{-foldMapOf :: (s ^~. a) m n -> (a -> m) -> s -> n-}
foldMapOf = _Forget

foldOf :: (Forget a a a -> Forget a s s) -> s -> a
{-foldOf :: s ^~~. a -> s -> a-}
foldOf = view

-- | Synonym for foldOf
view :: (Forget a a b -> Forget m s t) -> s -> m
view = viewWith' (\x -> x)

view' :: Pure f => (Forget (f a) a a -> Forget n s s) -> s -> n
view' = viewWith pure


viewWith :: (a -> m) -> (Forget m a a -> Forget n s s) -> s -> n
viewWith f l = foldMapOf l f

viewWith' :: (a -> m) -> (Forget m a b -> Forget n s t) -> s -> n
viewWith' f l = case l (Forget f) of Forget g -> g
