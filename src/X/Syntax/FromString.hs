module X.Syntax.FromString (FromString(..),module X) where
import X.Type.Char as X
import Language.Haskell.TH.Syntax (OccName(..),PkgName(..),ModName(..))

class FromString s where fromString :: [Char] -> s
instance FromString [Char] where fromString s = s
instance FromString OccName where fromString = OccName
instance FromString PkgName where fromString = PkgName
instance FromString ModName where fromString = ModName
