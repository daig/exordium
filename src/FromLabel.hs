{-# language TemplateHaskell #-}
{-# language NoMonomorphismRestriction #-}
{-# language QuasiQuotes #-}
module FromLabel where
import qualified Data.Map as M
import Symbol.Type as X
import Int.I
import Lens.Class as X
import Traversal0.Class as X
import Prism.Class as X
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Maybe.Type
import qualified Prelude as P
import qualified Data.List as P
import qualified Control.Monad as P
{-import Language.Haskell.TH.Lib (DecsQ)-}
import qualified Language.Haskell.TH.Ppr as P

class FromLabel (x :: Symbol) c s t a b | x s -> c, x t -> c, s -> a, t -> b, s b -> t, t a -> s where
  type FromLabelC x s :: (* -> *) -> Constraint
  fromLabel :: c p => p a b -> p s t

instance FromLabel "fst" Lens (a,x,y) (b,x,y) a b where
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
