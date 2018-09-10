{-# OPTIONS_HADDOCK not-home #-}
{-# language CPP #-}
module Types (module Types, module X) where
 {-(I8# -- $unsafe-}
 {-,I16#-}
 {-,I32#-}
 {-,I64#-}
 {-,Isize#-}
 {-,U8#-}
 {-,U16#-}
 {-,U32#-}
 {-,U64#-}
 {-,Usize#-}
 {-,Bool#-}
 {-,RuntimeRep(..)) where-}
#include "MachDeps.h"
import GHC.Prim
import GHC.Types as X (RuntimeRep(..))
import qualified Prelude
import qualified GHC.Int as GHC

print i = Prelude.print (GHC.I# i)
type PairR r1 r2 = TupleRep '[r1,r2]
type (a :: TYPE r1) * (b :: TYPE r2) = (# a, b #)
type BoolR = IntRep
type Bool = Bool#
type Bool# = Int#

-- | GHC does not enforce this type, so the operations assume they are passed a value within the valid range, but will correctly narrow the return type value.
type I8# = Int#

type I16# = Int#

#if WORD_SIZE_IN_BITS < 32
type I32# = Int32#
#else
type I32# = Int#
#endif


#if WORD_SIZE_IN_BITS < 64
type I64# = Int64#
#else
type I64# = Int#
#endif

type IsizeR = IntRep
type Isize# = Int#

type U8# = Word#
type U16# = Word#

#if WORD_SIZE_IN_BITS < 32
type U32# = Word32#
#else
type U32# = Word#
#endif


#if WORD_SIZE_IN_BITS < 64
type U64# = Word64#
#else
type U64# = Word#
#endif

type UsizeR = WordRep
type Usize# = Word#

-- $unsafe
-- GHC does not enforce this type, so the operations assume they are passed a value within the valid range, but will correctly narrow the return type value.
