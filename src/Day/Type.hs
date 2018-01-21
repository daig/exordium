module Day.Type where

data Day f g a = forall x y. Day (f x) (g y) (x -> y -> a)
