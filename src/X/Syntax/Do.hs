module X.Syntax.Do where

-- | A record to hold all the identifiers needed for [do notation](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14) under [-XRebindableSyntax](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RebindableSyntax) (possibly also with [-XApplicativeDo](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#applicative-do-notation)).
--
-- Use together with [-XRecordWildCards](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-RecordWildCards) to efficiently select syntax, either as a toplevel declaration applying to the whole module
--
-- > DoSyntax {..} = myDoSyntax
--
-- or locally within an expression, shaddowing the existing syntax
--
-- > let DoSyntax {..} = myDoSyntax in do
-- >    x <- fx
-- >    y <- fy
-- >    foo x y
data DoSyntax map return andThen ap bind fail = DoSyntax {fmap :: map
                                                    ,return :: return
                                                    ,(>>) :: andThen
                                                    ,(<*>) :: ap
                                                    ,(>>=) :: bind
                                                    ,fail :: fail}
-- | We want to store polymorphic functions in @DoSyntax@, (for example @return :: forall m. Monad m => a -> m a@)
-- which requires [-XImpredicativeTypes](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ImpredicativeTypes).
-- Unfortunately, @ImpredicativeTypes@ is broken, so we cannot get an impredicative @DoSyntax@ simply by applying its constructor. We CAN however make an impredicative tuple by giving explicit type annotations. Then, using @mkDoSyntax@, recover an appropriately impredicative @DoSyntax@
mkDoSyntax :: (a,b,c,d,e,f) -> DoSyntax a b c d e f
mkDoSyntax (a,b,c,d,e,f) = DoSyntax a b c d e f
