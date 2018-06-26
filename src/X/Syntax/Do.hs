{-# language RecordWildCards #-}
module X.Syntax.Do where
import qualified X.Functor.Monad as Mon
import qualified Prelude as P
import qualified Data.Map as M


data DoSyntax return andThen ap map bind = DoSyntax {return :: return
                                                    ,(>>) :: andThen
                                                    ,(<*>) :: ap
                                                    ,fmap :: map
                                                    ,(>>=) :: bind}
defaultDoSyntax = DoSyntax {return = M.singleton (999  :: P.Integer)
                           ,(>>) = M.unionWith (P.++)
                           ,(<*>) = ()
                           ,fmap = M.map
                           ,(>>=) = ()}
type family Foo f where
  Foo () = forall a. a -> a
{-ff = let DoSyntax {..} = defaultDoSyntax in do -}
  {-return @P.Integer ("a"::P.String)-}
  {-return ("b"::P.String)-}
{-return :: Pure f => a -> f a-}
{-return = pure-}

{-(>>) :: Apply f => f a -> f b -> f b-}
{-fa >> fb = map (\_ b -> b) fa `ap` fb-}

{-(<*>) :: Apply f => f (a -> b) -> f a -> f b-}
{-(<*>) = ap-}

{-fmap :: Map f => (a -> b) -> f a -> f b-}
{-fmap = map-}

{-(>>=) :: Bind m => m a -> (a -> m b) -> m b-}
{-m >>= f = bind f m-}
