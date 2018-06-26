{-# language MagicHash #-}
module X.Data.Bool (Bool(T,F),isTrue#
-- * utilities (TODO: refactor this)
                   ,not, otherwise) where
import GHC.Types (Bool(..),isTrue#)
import GHC.Base (otherwise) -- Currently the T,F patterns dont work for coverage checking in pattern guards


pattern T :: Bool
pattern T = True
pattern F :: Bool
pattern F = False
{-# complete T, F #-}

not :: Bool -> Bool
not = \case {T -> F; F -> T}
{-# inline not #-}
