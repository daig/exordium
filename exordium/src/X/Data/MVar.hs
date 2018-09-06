{-# language MagicHash #-}
module X.Data.MVar (MVar(..), type MVar#) where
import X.Prim.IO.MVar

data MVar s a = MVar# (MVar# s a)
