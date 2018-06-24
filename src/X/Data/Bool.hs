{-# language MagicHash #-}
module X.Data.Bool (Bool(T,F),isTrue#) where
import GHC.Types (Bool(..),isTrue#)


pattern T :: Bool
pattern T = True
pattern F :: Bool
pattern F = False

{-# complete T, F #-}
