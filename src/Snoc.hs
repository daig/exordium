module Snoc (module Snoc, module X) where
import Snoc.Class as X
import Maybe
import AFold
import Prism

pattern (:>) :: Snoc s a a s => s -> a -> s
pattern s :> a <- (foldMapOf _Snoc Just -> Just (s,a))
  where s :> a = review _Snoc (s,a)
