{-# OPTIONS_GHC -Wno-orphans #-}
module List (module X) where
import GHC.Base as X (build,augment,mapFB)
import qualified GHC.Base as Base (map,foldr)
import Map as X
import FoldMap as X

instance Map [] where map = Base.map
instance FoldMap [] where foldr = Base.foldr
