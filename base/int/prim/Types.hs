{-# language CPP #-}
{-# language TypeFamilies,  PolyKinds #-}
module Types where
#include "MachDeps.h"
import GHC.Prim
import GHC.Types (RuntimeRep(..))

type family Rep (a :: TYPE r) :: RuntimeRep where
  Rep Word32# = WordRep
  Rep Word64# = WordRep
  Rep Word# = WordRep
  Rep Int32# = Int64Rep
  Rep Int64# = Int64Rep
  Rep Int# = IntRep

type Bool# = Int#

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

type Usize# = Word#
