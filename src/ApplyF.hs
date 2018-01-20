module Class.FApply (module Class.FApply, module X) where

import Class.FPure as X

-- TODO: fix name
class FPure t => FAp t where fap :: t m (a -> b) -> t m a -> t m b
