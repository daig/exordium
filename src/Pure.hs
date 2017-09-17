module Pure where
import Map

class Map f => Pure f where
  pure :: a -> f a
  pure a = constMap a point
  point :: f ()
  point = pure ()
