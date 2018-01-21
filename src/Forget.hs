module Forget where
import Class.PlusZero as X
import Class.Lens as X
import Class.Map as X
import Class.Traversal as X
import Class.BiComap as X
import Class.Comap as X
import Type.K
import Utils.E
import Utils.Dimap

newtype Forget r a b = Forget (a -> r)
instance Lens (Forget r) where
  first (Forget z) = Forget (\(a,_) -> z a)
  traversal_ l (Forget ar) = Forget (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Dimap (Forget r) where
  dimap f _ (Forget z) = Forget (colmap f z)
instance CoLMap (Forget r) where colmap f (Forget z) = Forget (colmap f z)
instance RMap (Forget r) where rmap _ (Forget z) = Forget z
instance MapIso (Forget r a) where mapIso = map_mapIso
instance Map (Forget r a) where map = rmap_map
instance BiComap (Forget r) where
  bicomap f _ (Forget z) = Forget (colmap f z)
instance Comap (Forget r a) where comap = cormap
instance CoRMap (Forget r) where cormap _ (Forget z) = Forget z

instance Zero r => Prism (Forget r) where
  left (Forget z) = Forget (e'bifoldMap_ z (\_ -> zero))

instance PlusZero r => Traversal (Forget r) where
  traversal l (Forget ar) = Forget (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Zero r => Traversal0 (Forget r) where
  traversal0 l (Forget ar) = Forget (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
instance Plus r => Traversal1 (Forget r) where
  traversal1 l (Forget ar) = Forget (\s -> case (l (\a -> K (ar a))) s of {K r -> r})
