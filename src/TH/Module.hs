module TH.Module (Q.Module(TH.Module.Module), module X) where
import qualified Language.Haskell.TH.Syntax as Q
import TH.Name.Pkg as X
import TH.Name.Mod as X
import X.Optic.Prism

pattern Module :: PkgName -> ModName -> Q.Module
pattern Module{pkgName,modName} = Q.Module pkgName modName

