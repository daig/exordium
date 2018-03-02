module Rep.Type where
import I

type family Rep (p :: i -> j -> *) :: j -> *

type instance Rep (->) = I
