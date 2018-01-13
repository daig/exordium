module Applying where

import Dimap as X
import Lens as X (Lens)
import Lens 
import Compose as X (Compose)
import Compose

class Dimap p => Applying p where
  {-# minimal (@@@) | (***) #-}
  (@@@) :: p a (b -> c) -> p a b -> p a c
  f @@@ x = (\a -> (a,a)) >|(f *** x)|> (\(g,a) -> g a)
  (***) :: p a b -> p x y -> p (a,x) (b,y)
  {-pab *** pxy = (,) `postmap` premap (\(a,_) -> a) pab @@@ premap (\(_,x) -> x) pxy-}
  pab *** pxy = ((\(a,_) -> a) >|pab|> (,))
            @@@ ((\(_,x) -> x) >@ pxy       )

alongsideDefault :: (Lens p, Compose p) => p a b -> p x y -> p (a,x) (b,y)
alongsideDefault pab pxy = first pab < second pxy
