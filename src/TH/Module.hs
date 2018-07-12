module TH.Module (Q.Module(TH.Module.Module), Q.reifyModule, module X) where
import qualified Language.Haskell.TH.Syntax as Q
import TH.Name.Pkg as X
import TH.Name.Mod as X
import TH.Q as X

pattern Module :: PkgName -> ModName -> Q.Module
pattern Module{pkgName,modName} = Q.Module pkgName modName

thisModule :: Q Q.Module
thisModule = do
  Q.Loc {loc_package,loc_module} <- Q.location
  Q.returnQ (Module {pkgName = Q.PkgName loc_package
                    ,modName = Q.ModName loc_module})
