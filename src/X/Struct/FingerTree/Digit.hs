module X.Struct.FingerTree.Digit (module X.Struct.FingerTree.Digit,module X) where
import Prelude (Show,error)
import X.Functor.Fold as X
import X.Num.Measured as X
import X.Functor.Traverse as X
import X.ADT.Maybe
import X.ADT.Bool
import X.Functor.Map
import X.Mono.Cons
import X.Mono.Snoc

data Digit a = Digit1 ~a | Digit2 ~a ~a | Digit3 ~a ~a ~a | Digit4 ~a ~a ~a ~a deriving Show

instance Fold1 Digit where
  foldMap1 f (Digit1 a) = f a
  foldMap1 f (Digit2 a b) = f a `add` f b
  foldMap1 f (Digit3 a b c) = f a `add` f b `add` f c
  foldMap1 f (Digit4 a b c d) = f a `add` f b `add` f c `add` f d
instance Fold Digit where foldMap = foldMap1

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

splitDigit :: Measured a => (Measure a -> Bool) -> Measure a -> Digit a
                         -> (Maybe (Digit a), a, Maybe (Digit a))
splitDigit p !i = \case
  Digit1 a                -> (Nothing, a, Nothing)
  Digit2 a b     | p va   -> (Nothing, a, Just (Digit1 b))
                 | T   -> (Just (Digit1 a), b, Nothing)
   where va = i `add` measure a
  Digit3 a b c   | p va   -> (Nothing, a, Just (Digit2 b c))
                 | p vab  -> (Just (Digit1 a), b, Just (Digit1 c))
                 | T   -> (Just (Digit2 a b), c, Nothing)
   where (va,vab) = (i `add` measure a, va `add` measure b)
  Digit4 a b c d | p va   -> (Nothing, a, Just (Digit3 a b c))
                 | p vab  -> (Just (Digit1 a), b, Just (Digit2 c d))
                 | p vabc -> (Just (Digit2 a b), c, Just (Digit1 d))
                 | T   -> (Just (Digit3 a b c), c, Nothing)
   where (va,vab,vabc) = (i `add` measure a, va `add` measure b,vab `add` measure c)
