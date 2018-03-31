{-# language TemplateHaskell #-}
{-# language NoMonomorphismRestriction #-}
{-# language QuasiQuotes #-}
module X.Derive.FromLabel where
import X.Kind.Symbol as X
import X.Arrow.Traversed as X
{-import Language.Haskell.TH.Syntax-}
{-import X.Data.Maybe-}
{-import qualified Prelude as P-}
{-import qualified Language.Haskell.TH.Ppr as P-}
import X.Kind.Constraint

class FromLabel (x :: Symbol) c s t a b | x s -> c, x t -> c, s -> a, t -> b, s b -> t, t a -> s where
  type FromLabelC x s :: (* -> * -> *) -> Constraint
  fromLabel :: c p => p a b -> p s t

instance FromLabel "fst" Traversed_ (a,x,y) (b,x,y) a b where
  type FromLabelC "fst" (a,x,y) = Traversed_
  fromLabel = lens (\(a,_,_) -> a) (\(_,x,y) b -> (b,x,y))

{-mkClass' :: P.String -> Type -> Type -> Type -> Type -> [Dec] -> Dec-}
{-mkClass' x s t a b =-}
  {-InstanceD-}
    {-Nothing-}
    {-[]-}
    {-(ConT ''FromLabel `AppT` LitT (StrTyLit x) `AppT` s `AppT` t `AppT` a `AppT` b)-}
{-mkClass x s t a b =-}
  {-InstanceD-}
    {-Nothing-}
    {-[]-}
    {-(((((ConT ''FromLabel `AppT` LitT (StrTyLit x)) `AppT` s) `AppT` t) `AppT` a) `AppT` b)-}
