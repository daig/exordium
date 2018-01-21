module Rep.Type (Rep, module X) where
import I.Type as X (I)

type family Rep (p :: i -> j -> *) :: j -> *

type instance Rep (->) = I
