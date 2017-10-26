module Day where
import Prelude (Show(..))

data Day f g a = forall x y. Day (f x) (x -> y -> a) (g y)
type (:@:) = Day
pattern (:@:) x y p = Day x p y
