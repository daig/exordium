module Utils.Align (module Utils.Align, Plus, module X) where
import Class.Align as X
import Class.Plus

-- | Default definition for (+) @(f a)
alignWith_plus :: (Align f, Plus a) => f a -> f a -> f a
alignWith_plus = alignWith (\x -> x) (\x -> x) plus 
