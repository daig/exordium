{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- | It is best to import this module qualified to avoid ambiguous syntax.
--
-- > import qualified X.Syntax.Do as XDo
--
-- When imported qualified, the record selectors will work for @-XRecordWildCards@ bindings but won't collide with the syntax itself.
module X.Syntax.Do.X (DoSyntax(..),doSyntax,module X) where
import X.Syntax.Do 
import X.Functor.Monad as X (Monad,Apply,Pure,Map)
import X.Functor.Monad
import X.Type.Char
import X.Functor.Empty

-- | Do syntax using our 'Monad' superclasses: 'Map', 'Pure', 'Apply', and 'Bind'.
-- This means do expressions infer @(Bind m, Pure m)@ rather than @(Monad m)@ whenever possible.
-- To presuppose the monad laws, add an explicit annotation.
doSyntax = mkDoSyntax doTuple where
  doTuple :: ((forall f a b. Map f => (a -> b) -> f a -> f b)
             ,(forall f a  . Pure f => a -> f a)
             ,(forall f a b. Apply f => f a -> f b -> f b)
             ,(forall f a b. Apply f => f (a -> b) -> f a -> f b)
             ,(forall m a b. Bind m => m a -> (a -> m b) -> m b)
             ,(forall f a  . Empty f => [Char] -> f a))
  doTuple = (map
            ,pure
            ,(\fa fb ->  map (\_ b -> b) fa `ap` fb)
            ,ap
            ,(\m f -> bind f m)
            ,(\_ -> empty))
