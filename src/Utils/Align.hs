module Utils.Align (module Utils.Align, Plus, module X) where
import Align.Class as X
import Plus.Class

-- | Default definition for (+) @(f a)
alignWith_plus :: (Align f, Plus a) => f a -> f a -> f a
alignWith_plus = alignWith (\x -> x) (\x -> x) plus 
