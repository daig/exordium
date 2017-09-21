module Biempty where
import Bimap

class Biempty p where biempty :: p a b
