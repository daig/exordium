module X.Rep.Type where
import X.Type.I

type family Rep (p :: i -> j -> *) :: j -> *

type instance Rep (->) = I
