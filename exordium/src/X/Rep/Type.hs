module X.Rep.Type where
import X.Type.I
import X.Kind.Type

type family Rep (p :: i -> j -> Type) :: j -> Type

type instance Rep (->) = I
