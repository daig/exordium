module Class.FMap (module Class.FMap, module X) where
import Class.Map as X
import Class.Dimap as X
import NatTrans as X
import Type.Constraint as X

class FMap t where
  type FMapC t :: (* -> *) -> Constraint -- TODO: unsatisfied with name
  type FMapC t = Map -- TODO: merge with other transformer alass constraints?
  fmap :: FMapC t f => (f --> g) -> t f a -> t g a
class BimapF t where 
  type LFMapC t :: (* -> *) -> Constraint
  type LFMapC t = Map
  type RFMapC t :: (* -> *) -> Constraint
  type RFMapC t = Map
  bifmap :: (LFMapC t f, RFMapC t g) => (f --> f') -> (g --> g') -> t f g a -> t f' g' a
bifmap_fmap :: (BimapF t,LFMapC t f, RFMapC t g) => (g --> g') -> t f g a -> t f g' a
bifmap_fmap = bifmap (\x -> x)
