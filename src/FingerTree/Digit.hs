module FingerTree.Digit (Digit(..), module X) where
import Prelude (Show)
import FoldMap1 as X
import Measured as X
import Class.Traverse1 as X

data Digit a = Digit1 ~a | Digit2 ~a ~a | Digit3 ~a ~a ~a | Digit4 ~a ~a ~a ~a deriving Show

instance FoldMap1 Digit where
  foldMap1 f (Digit1 a) = f a
  foldMap1 f (Digit2 a b) = f a + f b
  foldMap1 f (Digit3 a b c) = f a + f b + f c
  foldMap1 f (Digit4 a b c d) = f a + f b + f c + f d
instance FoldMap Digit where foldMap = foldMap1

instance MapIso Digit where mapIso = map_mapIso
instance Map Digit where
  map f (Digit1 a) = Digit1 (f a)
  map f (Digit2 a b) = Digit2 (f a) (f b)
  map f (Digit3 a b c) = Digit3 (f a) (f b) (f c)
  map f (Digit4 a b c d) = Digit4 (f a) (f b) (f c) (f d)

instance Traverse1 Digit where
  traverse1 f (Digit1 a) = Digit1 `map` f a
  traverse1 f (Digit2 a b) = map Digit2 (f a) |$| f b
  traverse1 f (Digit3 a b c) = map Digit3 (f a) |$| f b |$| f c
  traverse1 f (Digit4 a b c d) = map Digit4 (f a) |$| f b |$| f c |$| f d
instance Traverse Digit where traverse = traverse1
  

instance Measured a => Measured (Digit a) where
  type Measure (Digit a) = Measure a
  measure = foldMap1 measure
