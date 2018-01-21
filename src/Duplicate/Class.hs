module Duplicate.Class (module Duplicate.Class, module X) where
import Apply.Class as X

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
  duplicate = extend (\wa -> wa)
  extend :: (w a -> b) -> w a -> w b
  extend f = \wa -> f `map` duplicate wa
