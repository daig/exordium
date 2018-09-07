module Eq where
import qualified GHC.Classes as GHC
import Bool

type Eq# = GHC.Eq
eq# :: Eq# a => a -> a -> Bool
{-# inline eq# #-}
eq# = (GHC.==)

ne# :: Eq# a => a -> a -> Bool
{-# inline ne# #-}
ne# = (GHC./=)
