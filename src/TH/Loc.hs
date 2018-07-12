module TH.Loc (Q.Loc(TH.Loc.Loc,loc_filename,loc_package,loc_module,loc_start,loc_end), Q.CharPos, Q.location) where
import qualified Language.Haskell.TH.Syntax as Q
import TH.Name.Mod as X
import TH.Name.Pkg as X

pattern Loc {filename,pkgName,modName,start,end}
  <- Q.Loc filename
           (PkgName -> pkgName)
           (ModName -> modName)
           start
           end
  where
  Loc f (PkgName p) (ModName m) s e = Q.Loc f p m s e
 
instance Q.Lift Q.Loc where
  lift Q.Loc {loc_filename, loc_package, loc_module, loc_start, loc_end}
    = [e|Loc $(Q.lift loc_filename) $(Q.lift loc_package) $(Q.lift loc_module) $(Q.lift loc_start) $(Q.lift loc_end)|]
