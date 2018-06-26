-- | Use this module with @-XRebindableSyntax@ (https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RebindableSyntax)
-- to override much default syntax
module X.Syntax (module X) where
import X.Syntax.FromInteger as X
import X.Syntax.Do as X

-- TODO: factor this into the prelude
