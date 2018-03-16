module Num.Eq (module X) where

import GHC.Classes as X (Eq(..))

-- Eq is the type of primitive structural equality
-- The instance should exactly match that which would be derived.
-- ie: Constructors should match exactly and recursively.
