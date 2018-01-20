module K (module X) where
import Class.Map as X
import Bimap as X
import Applicative as X
import Zero as X
import Comap as X
import Dimap as X
import Prism as X
import Type.E as X (E)
import Type.E
import K.Type as X
import Traverse as X
import Traverse0 as X
import FoldMap0 as X


instance FoldMap0 (K x) where foldMap0 _ _ = zero
instance Traverse0 (K x) where traverse0 f (K x) = pure (K x)
