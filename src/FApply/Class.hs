module FApply.Class (module FApply.Class, module X) where

import FPure.Class as X

-- TODO: fix name
class FPure t => FAp t where fap :: t m (a -> b) -> t m a -> t m b
