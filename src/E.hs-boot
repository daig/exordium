module E (E(..), module X) where
import Map as X
data E a b = L ~a | R ~b
instance Map (E a)
