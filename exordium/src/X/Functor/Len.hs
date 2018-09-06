module X.Functor.Len (Len(..), StaticLen(..), staticLen_len, module X) where
import X.Num.FromNatural as X
import X.Data.E
import X.Type.I
import X.Data.Maybe
import X.Data.These
import X.Type.Int.I
import X.Type.Word.W
import X.Type.K

class Len t where
  -- | Count the number of elements in a container
  len :: FromNatural n => t a -> n
  len = lenFrom (fromNatural 0)
  -- | Count the number of elements in a container starting from some base count
  lenFrom :: FromNatural n => n -> t a -> n
  lenFrom !n t = n `add` len t
  {-default len :: (StaticLen t,FromNatural n) => t a -> n-}
  {-len _ = staticLen @t-}
class Len t => StaticLen t where staticLen :: FromNatural n => n
staticLen_len :: forall t n a. (StaticLen t,FromNatural n) => t a -> n
staticLen_len _ = staticLen @t @n

instance Len (E a) where len = \case {L{} -> fromNatural 0; R{} -> fromNatural 1}
instance Len (These a) where len = \case {This{} -> fromNatural 0; That{} -> fromNatural 1; These{} -> fromNatural 1}
instance Len Maybe where len = \case {Nothing -> fromNatural 0; Just{} -> fromNatural 1}
instance StaticLen I where staticLen = fromNatural 1
instance Len I where len = staticLen_len

instance Len [] where
  {-# specialize len :: [a] -> Int #-}
  {-# specialize len :: [a] -> Word #-}
  {-# specialize len :: [a] -> Natural #-}
  len = \case {[] -> fromNatural 0; _:as -> fromNatural 1 `add` len as}

instance StaticLen (K a) where staticLen = fromNatural 0
instance Len (K a) where len = staticLen_len
instance StaticLen ((,) a) where staticLen = fromNatural 1
instance Len ((,) a) where len = staticLen_len
