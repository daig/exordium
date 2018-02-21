module Rep.Type where
import {-# source #-} I

type family Rep (p :: i -> j -> *) :: j -> *

type instance Rep (->) = I
