module Type.Rep (Rep, module X) where
import Type.I as X (I)

type family Rep (p :: i -> j -> *) :: j -> *

type instance Rep (->) = I
