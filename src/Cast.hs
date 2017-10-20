{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
module Cast where
import AllSatisfied
import Prism
import Iso
import AGetter

class (AllSatisfied (Preserves a b) a, AllSatisfied (Preserves a b) b) => a ~? b where
  {-# minimal tryCasted# | upcast#, tryDowncast# #-}
  type Preserves a b :: [* -> Constraint]
  type Preserves a b = '[]
  tryCasted# :: a +~~ b
  tryCasted# = prism' upcast# tryDowncast#
  upcast# :: b -> a
  tryDowncast# :: a -> (?) b
  default tryDowncast# :: a ~= b => a -> (?) b
  tryDowncast# = \x -> Some (downcast# x)

class (AllSatisfied (PreservesF f g) f, AllSatisfied (PreservesF f g) g) => f ~?: g where
  type PreservesF f g :: [(k -> *) -> Constraint]
  type PreservesF f g = '[]
  tryCastedF# :: (f a +~ g a) (g b) (f b)
  upcastF# :: g a -> f a
  tryDowncastF# :: f a -> (?) (f a)

class a ~? b => a ~= b where
  {-# minimal casted# | downcast# #-}
  casted# :: a =~~ b
  casted# = iso downcast# upcast#
  downcast# :: a -> b
  downcast# = view casted#

class f ~=: g where castedF# :: (f a =~ g a) (g b) (f b)
{-instance f ~=: g => f a ~= g a where casted# = castedF#-}
