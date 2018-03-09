module Optic.Edit where
import Map.Pro

newtype Edit a b s t = Edit {runEdit :: s -> b -> t}

instance Promap (Edit a b) where promap f g (Edit e) = Edit (\s' b -> g (e (f s') b))
instance ComapL (Edit a b) where colmap = (`promap` \x -> x)
instance MapR (Edit a b) where rmap = promap (\x -> x)
