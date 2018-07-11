module TH.Loc (Loc(..), CharPos, location) where
import Language.Haskell.TH.Syntax

instance Lift Loc where
  lift Loc {loc_filename, loc_package, loc_module, loc_start, loc_end}
    = [e|Loc $(lift loc_filename) $(lift loc_package) $(lift loc_module) $(lift loc_start) $(lift loc_end)|]
