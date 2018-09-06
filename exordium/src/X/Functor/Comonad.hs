{-# language UnboxedTuples #-}
module X.Functor.Comonad (Comonad, module X) where
import X.Functor.Fold as X
import X.Functor.Duplicate as X
import X.Type.I


-- | extend extract = id
-- extract < extend f = f
class (Fold_ w,Duplicate w) => Comonad w

instance Comonad ((,) x)
instance Comonad I

fold__nonempty :: Fold_ w => w X -> a
fold__nonempty w = case fold_ w of {}
fold__decide :: (Fold_ w, Map w) => w (E a b) -> E (w a) (w b)
fold__decide w = case fold_ w of
  L a -> L (map (\case L a' -> a'; R _ -> a) w)
  R b -> R (map (\case L _ -> b; R b' -> b') w)
