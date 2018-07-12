module TH.Name.Mod (ModName(..)) where
import Language.Haskell.TH.Syntax
import X.Optic.Iso
import X.Type.Char

_ModName_ :: Promap p => p [Char] [Char] -> p ModName ModName
_ModName_ = _coerce_
