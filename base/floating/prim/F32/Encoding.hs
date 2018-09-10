{-# OPTIONS_HADDOCK not-home #-}
{-# language GHCForeignImportPrim, CApiFFI, UnliftedFFITypes, ForeignFunctionInterface, DuplicateRecordFields #-}
-- | [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) encoding of 32-bit floats
module F32.Encoding where
import GHC.Prim as X
import GHC.Types as X (Int(..),Word(..),Float(..),IO,RuntimeRep(..))
import U8 as X (U8#)
import U32 hiding (Word)
import Usize (Usize#)
import Bool as X (Bool#)

-- * Types
type F32# = Float#
-- | Floats are encoded as word bitfields
type IEEE32# = U32#
-- | NaNs are a subtype of IEEE32#
type NaN32# = IEEE32#

-- * encoding


-- | Bitcast a float to its IEEE 754 format via a roundtrip through memory
foreign import prim "stg_floatToWord32zh" toIEEE :: F32# -> IEEE32#
-- | Bitcast an IEEE 754 value into a float via a roundtrip through memory
foreign import prim "stg_word32ToFloatzh" fromIEEE :: IEEE32# -> F32#

-- *** Bitfield encodings
-- | Decode an IEEE value into its sign, exponent, and mantissa
decode :: IEEE32# -> (# Bool#, U8#, U32# #)
{-# INLINE decode #-}
decode w = (# isNegative w, getExp w, getMantissa w #)

-- | Encode an IEEE value from its sign, exponent, and mantissa
encode :: Bool# -> U8# -> U32# -> IEEE32#
{-# INLINE encode #-}
encode (int2Word# -> s) e m
  = shiftL# s SIGN_OFF
  `or#` shiftL# e EXP_OFF
  `or#` and#    m MANTISSA_MASK

-- *** NaN encodings
-- | Decode a (assumed) NaN value into its sign, quiet flag, and payload
decodeNaN :: NaN32# -> (# Bool#, Bool#, U32# #)
decodeNaN w = (# isNegative w, isQuiet w, getPayload w #)
-- | Encode a NaN value from its sign, quiet flag, and payload
encodeNaN :: Bool# -> Bool# -> U32# -> IEEE32#
encodeNaN (int2Word# -> s) (int2Word# -> q) p
  = shiftL# s SIGN_OFF
  `or#` shiftL# q QUIET_OFF
  `or#` and#    p PAYLOAD_MASK
  `or#` EXP_MASK

-- * Accessors
getExp :: IEEE32# -> U8#
getExp w    = (w `and` EXP_MASK) `shiftR#` EXP_OFF
getMantissa :: IEEE32# -> U32#
getMantissa = and MANTISSA_MASK
isQuiet :: IEEE32# -> Bool#
isQuiet w        = word2Int# ((w `and` QUIET_MASK) `shiftR#` QUIET_OFF)
getPayload :: IEEE32# -> U32#
getPayload = and PAYLOAD_MASK
-- ** Predicates
isNegative,isFinite, isInfinite, isDenormalized, isNaN :: IEEE32# -> Bool#
isNegative w     = word2Int# ((w `and` SIGN_MASK) `shiftR#` SIGN_OFF)
isFinite w       = ((w `and#` EXP_MASK)     `U32.xor` EXP_MASK) `U32.ne` 0##
isInfinite w     = ((w `and#` NON_SIGN_MASK) `U32.xor` EXP_MASK) `U32.eq` 0##
isDenormalized w = ((w `and#` EXP_MASK)      `U32.eq` 0##)
                   `andI#` ((w `and#` MANTISSA_MASK) `U32.ne` 0##)
isNaN     w      = ((w `U32.and` NON_SIGN_MASK) `U32.xor`  EXP_MASK) `U32.le` MANTISSA_MASK

isInfinity,isNegInfinity,isNegZero :: IEEE32# -> Bool#
isInfinity w     = (w `and#` 0xFFFFFFFF##) `U32.eq` INFINITY
isNegInfinity w  = (w `and#` 0xFFFFFFFF##) `U32.eq` NEG_INFINITY
isNegZero w      = (w `U32.and` 0xFFFFFFFF##) `U32.eq` 0x80000000##

-- * Constants
-- ** Masks
pattern NON_SIGN_MASK, NAN_HEADER_MASK, EXP_MASK, SIGN_MASK, MANTISSA_MASK, QUIET_MASK, PAYLOAD_MASK :: U32#
pattern NON_SIGN_MASK   = 0x7FFFFFFF##
pattern NAN_HEADER_MASK = 0xFFC00000##
pattern EXP_MASK        = 0x7F800000##
pattern SIGN_MASK       = 0x80000000##
pattern MANTISSA_MASK   = 0x007FFFFF##
pattern QUIET_MASK      = 0x00400000##
pattern PAYLOAD_MASK    = 0x003FFFFF##
-- ** Values
pattern INFINITY, NEG_INFINITY :: IEEE32#
pattern INFINITY        = 0x7F800000##
pattern NEG_INFINITY    = 0xFF800000##
-- ** Offsets
pattern EXP_OFF, QUIET_OFF, SIGN_OFF :: Usize#
pattern EXP_OFF         = 23##
pattern QUIET_OFF       = 22##
pattern SIGN_OFF        = 31##
