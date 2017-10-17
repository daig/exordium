module Witness (W(..), type (=>.)(..), module X) where
import Constraint as X
import Category as X

data W c = c => W

withWitness :: W c -> (c => r) -> r
withWitness W = \r -> r
-- | A value @a =>. b@ is a witness to the fact that forall x, a x implies b x
data a =>. b = Sub (a => W b)
refl :: a =>. a
refl = Sub W
-- | Contravariantly map an entailment over an environment
comapEnv :: a =>. b -> (b => r) -> (a => r)
comapEnv w r = case w of Sub W -> r
infixr `comapEnv`
instance Compose (=>.) where f < g = Sub (g `comapEnv` f `comapEnv` W)
instance Category (=>.) where id = Sub W
