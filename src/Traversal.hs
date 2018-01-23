module Traversal (module Traversal, module X) where
import Traversal.Class as X
import Int
import Star.Type
import Indexable.Class

traversed_dimap :: Traversal p => (a -> x) -> (y -> b) -> p x y -> p a b
traversed_dimap f g = traversal (\xfy a -> map g (xfy (f a)))

traverseOf :: Applicative f => (Star f a b -> Star f s t) -> (a -> f b) -> s -> f t
traverseOf l afb s = case l (Star afb) of Star sft -> sft s

itraverseOf :: Applicative f => (IndexingP (Star f) a b -> IndexingP (Star f) s t) -> (Int -> a -> f b) -> s -> f t
itraverseOf l iafb = runStar (indexP (l (IndexingP (\i -> (i `plus` (1::Int),Star (iafb i))))) (0::Int))

itraverseOf' :: (IFun f Int a b -> IFun f Int s t) -> (Int -> a -> f b) -> s -> f t
itraverseOf' l iafb = case l (IFun iafb) of IFun isft -> isft 0
