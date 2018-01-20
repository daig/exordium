module Class.Duplicate (module Class.Duplicate, module X) where
import Class.Apply as X

-- | associativity of duplicate:
-- duplicate < duplicate = map duplicate < duplicate
-- or equivalently
-- extend f < extend g = extend (f < extend g)
--
-- Distribution over |$|:
-- duplicate (f |$| a) = duplicate f |$(|$|)$| duplicate a
class Apply w => Duplicate w where
  {-# minimal duplicate | extend #-}
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = \wa -> f `map` duplicate wa
