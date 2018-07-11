-- | For "global" names ('NameG') we need a totally unique name,
-- so we must include the name-space of the thing
-- 
-- For unique-numbered things ('NameU'), we've got a unique reference
-- anyway, so no need for name space
-- 
-- For dynamically bound thing ('NameS') we probably want them to
-- in a context-dependent way, so again we don't want the name
-- space.  For example:
-- 
-- > let v = mkName "T" in [| data $v = $v |]
-- 
-- Here we use the same Name for both type constructor and data constructor
-- 
-- 
-- NameL and NameG are bound *outside* the TH syntax tree
-- either globally (NameG) or locally (NameL). Ex:
-- 
-- > f x = $(h [| (map, x) |])
-- 
-- The 'map' will be a NameG, and 'x' wil be a NameL
-- 
-- These Names should never appear in a binding position in a TH syntax tree
--
-- {- $namecapture #namecapture#
-- much of 'Name' API is concerned with the problem of /name capture/, which
-- can be seen in the following example.
--
-- > f expr = [| let x = 0 in $expr |]
-- > ...
-- > g x = $( f [| x |] )
-- > h y = $( f [| y |] )
--
-- a naive desugaring of this would yield:
--
-- > g x = let x = 0 in x
-- > h y = let x = 0 in y
--
-- all of a sudden, @g@ and @h@ have different meanings! In this case,
-- we say that the @x@ in the RHS of @g@ has been /captured/
-- by the binding of @x@ in @f@.
--
-- What we actually want is for the @x@ in @f@ to be distinct from the
-- @x@ in @g@, so we get the following desugaring:
--
-- > g x = let x' = 0 in x
-- > h y = let x' = 0 in y
--
-- which avoids name capture as desired.
--
-- In the general case, we say that a @Name@ can be captured if
-- the thing it refers to can be changed by adding new declarations.
module TH.Name
  (Name(TH.Name.Name,occName,nameFlavour)
  ,module X
  ) where
import Language.Haskell.TH.Syntax as X (type Name
                                       ,newName, mkName
                                       )
import TH.Q as X
import TH.Name.Occ as X
import TH.Name.Flavour as X
import X.Type.Char as X
{-import qualified Language.Haskell.TH as Q-}
import qualified Language.Haskell.TH.Syntax as Q
{-import X.Syntax.FromString-}
import X.Data.Maybe
import X.Optic.Prism

pattern Name :: OccName
             -> NameFlavour
             -> Name
pattern Name{occName, nameFlavour} = Q.Name occName nameFlavour

_Name'module = lens0' get set where
  get = \case
    Name _ (NameQ (ModName s)) -> Just s
  set = set

