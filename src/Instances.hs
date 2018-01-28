module Instances (defaulting,instances,syncTH) where
import Language.Haskell.TH.Instances
import Language.Haskell.TH.Syntax
import Prelude

syncTH :: Q [Dec]
syncTH = return []
