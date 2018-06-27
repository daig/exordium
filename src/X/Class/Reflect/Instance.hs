{-# language MagicHash #-}
{-# language DuplicateRecordFields #-}
{-# language QuantifiedConstraints #-}
{-# language UndecidableInstances #-}
module X.Class.Reflect.Instance (module X.Class.Reflect.Instance, module X) where
import X.Class.Reflect as X
import X.Num.Add0
import X.Num.Mul
import X.Syntax.FromInteger
import X.Cast.Coerce
import X.Functor.Map

-- | A value of type @a@, in the context of an instance dictionary @s@ for class @c@
newtype Reflected c a s = Reflected a
  deriving newtype FromInteger#
-- | Opaque type variable @s@ reflects a reified instance of @c@ for @a@
type Instance c a s = Reflect s (Reified c a)

class (forall a s. Instance c a s => c (Reflected c a s))
  => Reify (c :: Type -> Constraint) where
  data Reified   c :: Type         -> Type -- TODO: generalize
reifyInstance :: (Reify c, Map f)
               => Reified c a 
               -> (forall (s :: Type). Instance c a s => f (Reflected c a s))
               -> f a
reifyInstance i fa = reify i (\p -> map# (unReflected p) fa) where
  unReflected :: proxy s -> Reflected c a s -> a
  unReflected _ t = coerce t

withInstance :: (Reify c, Map f)
             => (forall (s :: Type). Instance c a s => f (Reflected c a s))
             -> Reified c a 
             -> f a
withInstance fa i = reifyInstance i fa


-- TODO: derive these instances with TH

instance Reify Zero where
  data Reified Zero a = Zero {__zero__ :: a}
instance Reflect s (Reified Zero a) => Zero (Reflected Zero a s) where
  zero = reflectResult (\(Zero {__zero__}) -> coerce __zero__)

instance Reify Add where
  data Reified Add a = Add {__add__ :: a -> a -> a}
instance Reflect s (Reified Add a) => Add (Reflected Add a s) where
  add x y = reflectResult (\(Add {__add__}) -> coerce __add__ x y)

instance Reify Add0 where
  data Reified Add0 a = Add0 {__add__ :: a -> a -> a, __zero__ :: a}
instance Reflect s (Reified Add0 a) => Add (Reflected Add0 a s) where
  add x y = reflectResult (\Add0 {__add__} -> coerce __add__ x y)
instance Reflect s (Reified Add0 a) => Zero (Reflected Add0 a s) where
  zero = reflectResult (\Add0 {__zero__} -> coerce __zero__)
instance Reflect s (Reified Add0 a) => Add0 (Reflected Add0 a s)
