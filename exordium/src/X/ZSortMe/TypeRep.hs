module X.ZSortMe.TypeRep (module X) where

import GHC.Fingerprint.Type as X

import Data.Typeable as X
  (TypeRep, TyCon
  ,tyConPackage, tyConModule, tyConName, rnfTyCon, tyConFingerprint
  ,rnfTypeRep,showsTypeRep,mkFunTy
  ,funResultTy, splitTyConApp, typeRepArgs
  ,typeRepTyCon, typeRepFingerprint)

--TODO: move rnfTyCon and rnfTypeRep into class method
