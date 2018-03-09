module Snoc (module Snoc, module X) where
import Snoc.Class as X
import Maybe
import Optic.Review
import Optic.View

pattern (:>) :: Snoc s a a s => s -> a -> s
pattern s :> a <- (_View _Snoc Just -> Just (s,a))
  where s :> a = _Review _Snoc (s,a)
