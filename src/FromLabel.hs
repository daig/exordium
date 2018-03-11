{-# language TemplateHaskell #-}
{-# language NoMonomorphismRestriction #-}
{-# language QuasiQuotes #-}
module FromLabel where
import qualified Data.Map as M
import Symbol as X
import Int.I
import Traversed as X
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
