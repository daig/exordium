module FingerTree.Digit (Digit(..), lheadDigit,ltailDigit, rheadDigit,rtailDigit,module X) where
import Prelude (Show,error)
import FoldMap1.Class as X
import Measured.Class as X
import Traverse1.Class as X
import Map

data Digit a = Digit1 ~a | Digit2 ~a ~a | Digit3 ~a ~a ~a | Digit4 ~a ~a ~a ~a deriving Show

instance FoldMap1 Digit where
  foldMap1 f (Digit1 a) = f a
  foldMap1 f (Digit2 a b) = f a `plus` f b
  foldMap1 f (Digit3 a b c) = f a `plus` f b `plus` f c
  foldMap1 f (Digit4 a b c d) = f a `plus` f b `plus` f c `plus` f d
instance FoldMap Digit where foldMap = foldMap1

instance MapIso Digit where mapIso = map_mapIso
instance Map Digit where
  map f (Digit1 a) = Digit1 (f a)
  map f (Digit2 a b) = Digit2 (f a) (f b)
  map f (Digit3 a b c) = Digit3 (f a) (f b) (f c)
  map f (Digit4 a b c d) = Digit4 (f a) (f b) (f c) (f d)

instance Traverse1 Digit where
  traverse1 f (Digit1 a) = Digit1 `map` f a
  traverse1 f (Digit2 a b) = map Digit2 (f a) `ap` f b
  traverse1 f (Digit3 a b c) = map Digit3 (f a) `ap` f b `ap` f c
  traverse1 f (Digit4 a b c d) = map Digit4 (f a) `ap` f b `ap` f c `ap` f d
instance Traverse Digit where traverse = traverse1
  

instance Measured a => Measured (Digit a) where
  type Measure (Digit a) = Measure a
  measure = foldMap1 measure

lheadDigit :: Digit a -> a
lheadDigit (Digit1 a) = a
lheadDigit (Digit2 a _) = a
lheadDigit (Digit3 a _ _) = a
lheadDigit (Digit4 a _ _ _) = a

ltailDigit :: Digit a -> Digit a
ltailDigit (Digit1 _) = error "ltailDigit"
ltailDigit (Digit2 _ b) = Digit1 b
ltailDigit (Digit3 _ b c) = Digit2 b c
ltailDigit (Digit4 _ b c d) = Digit3 b c d


rheadDigit :: Digit a -> a
rheadDigit (Digit1 a) = a
rheadDigit (Digit2 _ b) = b
rheadDigit (Digit3 _ _ c) = c
rheadDigit (Digit4 _ _ _ d) = d

rtailDigit :: Digit a -> Digit a
rtailDigit (Digit1 _) = error "rtailDigit"
rtailDigit (Digit2 a _) = Digit1 a
rtailDigit (Digit3 a b _) = Digit2 a b
rtailDigit (Digit4 a b c _) = Digit3 a b c
