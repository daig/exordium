{-# language MagicHash #-}
{-# language UndecidableSuperClasses #-}
{-# language UnboxedTuples #-}
module X.Class.Thaw where
import X.Prim.IO
import X.Type.IO
import X.Type.ST
import X.Kind.Type
import X.Prim.Array

class Thaw (f :: k) where
  data Mutable1 f :: Type -> k
  thaw1# :: f a -> State# s -> (# State# s, Mutable f s a #)
  freeze1# :: Mutable f s a -> State# s -> (# State# s, f a #)

data Array a = Array# (Array# a)
instance Thaw Array where
  data Mutable Array s a = MutableArray# (MutableArray# s a)
  thaw# (Array# a) s = case thawArray# a 0# (sizeofArray# a) s of
    (# s', ma #) -> (# s', MutableArray# ma #)
