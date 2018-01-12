module Lift where
import Map

class Lift t where lift :: Map f => f a -> t f a
