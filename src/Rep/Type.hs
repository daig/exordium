module Rep.Type where
import Type.I

type family Rep (p :: i -> j -> *) :: j -> *

type instance Rep (->) = I
