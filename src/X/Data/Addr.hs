{-# language MagicHash #-}
module X.Data.Addr (Addr(..),type Addr#) where
import X.Prim.Addr

-- | A machine address
data Addr = Addr# Addr# -- deriving ( Typeable )

{-instance Eq Addr where-}
  {-Addr a# == Addr b# = eqAddr# a# b#-}
  {-Addr a# /= Addr b# = neAddr# a# b#-}

{-instance Ord Addr where-}
  {-Addr a# > Addr b# = gtAddr# a# b#-}
  {-Addr a# >= Addr b# = geAddr# a# b#-}
  {-Addr a# < Addr b# = ltAddr# a# b#-}
  {-Addr a# <= Addr b# = leAddr# a# b#-}

-- TODO: add class for remAddr#
