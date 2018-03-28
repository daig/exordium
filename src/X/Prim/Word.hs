{-# language MagicHash #-}
--------------------------------------------------------------------
-- |
-- Module    :  Prim.Word 
-- Copyright :  (c) <Dai> 2017
-- License   :  MIT
-- Maintainer:  <dailectic@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- aaa
--------------------------------------------------------------------
module X.Prim.Word
  (
  -- * The Word Size Story
  -- $wordsize
  Word# -- | Word literals are written like @3##@

  -- * Arithmetic
  ,plusWord#, minusWord#, timesWord#
  ,plusWord2# -- | Returns @(\# high, low \#)@ (or equivalently, @(\# carry, low \#)@)
  ,subWordC#
  ,timesWord2# -- | Returns @(\# high, low \#)@
  ,quotWord#, remWord#, quotRemWord#
  , quotRemWord2#
  -- | Takes high word of dividend, then low word of dividend, then divisor.
  -- Requires that high word is not divisible by divisor.

  -- * Comparisons
  ,gtWord#, geWord#, eqWord#, neWord#, ltWord#, leWord#
  -- * Conversions
  ,int2Word#, word2Int#, word2Float#, word2Double#
  -- ** Narrowing
  -- $narrow 
  ,narrow8Word#, narrow16Word#, narrow32Word#
  -- * Bitwise Logic
  ,and#, or#, xor#, not#
  -- * Bitshifts
  ,uncheckedShiftL#, uncheckedShiftRL#
  -- * Bitwise Folds
  -- ** Population Count
  ,popCnt8#, popCnt16#, popCnt32#, popCnt64#, popCnt#
  -- ** Leading Zeroes
  ,clz8#, clz16#, clz32#, clz64#, clz#
  -- ** Trailing Zeroes
  ,ctz8#, ctz16#, ctz32#, ctz64#, ctz#
  -- * Byte Swaps
  ,byteSwap16#, byteSwap32#, byteSwap64#, byteSwap#
  -- * Re-exported Types
  ,module X 
  ) where
import GHC.Prim
import X.Prim.Int as X (Int#)
import X.Prim.Float as X (Float#)
import X.Prim.Double as X (Double#)

-- $wordsize
--       Haskell98 specifies that signed integers (type 'Type.Int.Int')
--       must contain at least 30 bits. GHC always implements 
--       Int using the primitive type  'Int#', whose size equals
--       the  MachDeps.h constant  WORD\_SIZE_IN_BITS.
--       This is normally set based on the  config.h parameter
--        SIZEOF_HSWORD, i.e., 32 bits on 32-bit machines, 64
--       bits on 64-bit machines.  However, it can also be explicitly
--       set to a smaller number, e.g., 31 bits, to allow the
--       possibility of using tag bits. Currently GHC itself has only
--       32-bit and 64-bit variants, but 30 or 31-bit code can be
--       exported as an external core file for use in other back ends.
--
--       GHC also implements a primitive unsigned integer type 
--       'Word#' which always has the same number of bits as 
--       'Int#'.
--
--       In addition, GHC supports families of explicit-sized integers
--       and words at 8, 16, 32, and 64 bits, with the usual
--       arithmetic operations, comparisons, and a range of
--       conversions.  The 8-bit and 16-bit sizes are always
--       represented as  'Int#' and  'Word'#, and the
--       operations implemented in terms of the primops on these
--       types, with suitable range restrictions on the results (using
--       the  <Prim-Int.html#narrow narrow_n_Int#> and  <#narrow narrow_n_word#> families
--       of primops.  The 32-bit sizes are represented using 
--       'Int#' and  'Word#' when  WORD_SIZE_IN_BITS
--       >= 32; otherwise, these are represented using distinct
--       primitive types  'Int32#' and  'Word32#'. These (when
--       needed) have a complete set of corresponding operations;
--       however, nearly all of these are implemented as external C
--       functions rather than as primops.  Exactly the same story
--       applies to the 64-bit sizes.  All of these details are hidden
--       under the  PrelInt and  PrelWord module X.s, which use
--        #if-defs to invoke the appropriate types and
--       operators.

--       Word size also matters for the families of primops for
--       indexing/reading/writing fixed-size quantities at offsets
--       from an array base, address, or foreign pointer.  Here, a
--       slightly different approach is taken.  The names of these
--       primops are fixed, but their  types vary according to
--       the value of  WORD\_SIZE\_IN\_BITS. For example, if word
--       size is at least 32 bits then an operator like
--       indexInt32Array# has type  ByteArray# -> Int#
--       -> Int#; otherwise it has type  ByteArray# -> Int# ->
--       Int32#.  This approach confines the necessary 
--       #if-defs to this file; no conditional compilation is needed
--       in the files that expose these primops.

--       Finally, there are strongly deprecated primops for coercing
--       between  Addr#, the primitive type of machine
--       addresses, and  Int#.  These are pretty bogus anyway,
--       but will work on existing 32-bit and 64-bit GHC targets; they
--       are completely bogus when tag bits are used in  Int#,
--       so are not available in this case. 


-- $narrow
-- #narrow#

-- $quotRemWord2
-- | Takes high word of dividend, then low word of dividend, then divisor.
-- Requires that high word is not divisible by divisor.
