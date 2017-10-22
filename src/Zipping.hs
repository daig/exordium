module Zipping where
import Choice as X
import Strong as X
import Distributive
import Map

newtype ZippingF f a b = ZippingF {runZippingF :: f a -> b}
instance Map f => Dimap (ZippingF f) where dimap f g (ZippingF z) = ZippingF (dimap (map f) g z)
instance Map f => ComapL (ZippingF f) where comapl = premap
instance Map f => MapR (ZippingF f) where mapr = postmap
instance Map f => Map (ZippingF f a) where map = postmap
{-instance Strong (ZippingF f) where first (ZippingF z) = first z-}


newtype Zipping a b = Zipping {runZipping :: a -> a -> b}
data V2 a = V2 a a 
