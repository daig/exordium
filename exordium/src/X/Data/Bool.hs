{-# language MagicHash #-}
module X.Data.Bool (Bool(T,F),isTrue#
-- * utilities (TODO: refactor this)
                   ,not, otherwise,and,or
                   ) where
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

and :: Bool -> Bool -> Bool
infixr 3 `and`
and F _ = F
and _ F = F
and T T = T

or :: Bool -> Bool -> Bool
infixr 2 `or`
or T _ = T
or _ T = T
or F F = F
