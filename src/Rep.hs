module Rep (Rep, module X) where
import I as X

type family Rep (p :: i -> j -> *) :: j -> *

type instance Rep (->) = I
