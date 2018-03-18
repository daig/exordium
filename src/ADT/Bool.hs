module ADT.Bool (Bool(T,F)) where
import GHC.Types (Bool(..))

pattern T :: Bool
pattern T = True
pattern F :: Bool
pattern F = False

{-# complete T, F #-}
