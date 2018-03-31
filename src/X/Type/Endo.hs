module X.Type.Endo where
import X.Arrow.Folded
import X.Arrow.Precoerce
import X.Functor.Comap
import X.Arrow.Category
import X.Functor.Append

newtype Endo p a = Endo {runEndo :: p a a}
instance Folded_ p => Comap (Endo p) where
  comap f (Endo p) = Endo (folding_ f p)
instance Precoerce p => Map (Endo p) where
  map f (Endo p) = Endo (from f p)
instance Promap p => Remap (Endo p) where
  remap f g (Endo p) = Endo (promap f g p)
instance Compose p => Append (Endo p) where
  append (Endo p) (Endo q) = Endo (precompose p q)
{-instance Category p => Add--}
