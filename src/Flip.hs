module Flip (Flip(..), module X) where
import Map as X

newtype Flip f b a = Flip (f a b) 
