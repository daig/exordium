module Map.Co.L where

class ComapL f where colmap :: (a -> b) -> f b x -> f a x

instance ComapL (->) where colmap ab bx a = bx (ab a)
