module Arrow.Postcoerce (Postcoerce(..), postcoerce__2, module X) where
import Arrow.Traversed as X

class Traversed_ p => Postcoerce p where
  {-# minimal to | postcoerce #-}
  to :: (s -> a) -> p a b -> p s t
  to sa p = postcoerce (premap sa p)
  postcoerce :: p x b -> p x t
  postcoerce = to (\x -> x)

postcoerce__2 :: Postcoerce p => p a b -> p (x,a) (x,b)
postcoerce__2 = to (\(_,a) -> a)
