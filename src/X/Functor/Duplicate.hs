module X.Functor.Duplicate (Duplicate(..), module X) where
import X.Functor.Apply as X

-- | associativity of duplicate:
-- duplicate < duplicate = map duplicate < duplicate
-- or equivalently
-- extend f < extend g = extend (f < extend g)
class Map w => Duplicate w where
  {-# minimal duplicate | extend #-}
  duplicate :: w a -> w (w a)
  duplicate = extend (\wa -> wa)
  extend :: (w a -> b) -> w a -> w b
  extend f = \wa -> f `map` duplicate wa

-- | Distribution over |$|:
-- duplicate (f |$| a) = duplicate f |$(|$|)$| duplicate a
class (Apply w, Duplicate w) => DuplicateApply w

instance Duplicate ((,) x) where duplicate xa = constMap xa xa
