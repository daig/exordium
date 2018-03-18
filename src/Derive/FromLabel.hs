{-# language TemplateHaskell #-}
{-# language NoMonomorphismRestriction #-}
{-# language QuasiQuotes #-}
module Derive.FromLabel where
import qualified Data.Map as M
import Kind.Symbol as X
import Type.Int.I
import Arrow.Traversed as X
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Maybe
import qualified Prelude as P
import qualified Data.List as P
import qualified Control.Monad as P
import qualified Language.Haskell.TH.Ppr as P
import Kind.Constraint

class FromLabel (x :: Symbol) c s t a b | x s -> c, x t -> c, s -> a, t -> b, s b -> t, t a -> s where
  type FromLabelC x s :: (* -> *) -> Constraint
  fromLabel :: c p => p a b -> p s t

instance FromLabel "fst" Traversed_ (a,x,y) (b,x,y) a b where
  fromLabel = lens (\(a,x,y) -> a) (\(a,x,y) b -> (b,x,y))

mkClass' :: P.String -> Type -> Type -> Type -> Type -> [Dec] -> Dec
mkClass' x s t a b =
  InstanceD
    Nothing
    []
    (ConT ''FromLabel `AppT` LitT (StrTyLit x) `AppT` s `AppT` t `AppT` a `AppT` b)
mkClass x s t a b =
  InstanceD
    Nothing
    []
    (((((ConT ''FromLabel `AppT` LitT (StrTyLit x)) `AppT` s) `AppT` t) `AppT` a) `AppT` b)
