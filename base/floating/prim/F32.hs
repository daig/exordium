{-# language GHCForeignImportPrim, CApiFFI, UnliftedFFITypes, ForeignFunctionInterface, DuplicateRecordFields #-}
module F32 (U32#, module F32, module X) where
import GHC.Prim as X
import GHC.Types as X (Int(..),Word(..),Float(..),IO,RuntimeRep(..))
{-import qualified Prelude-}
{-import qualified GHC.Float as GHC-}
import U32 hiding (Word)
import I32 (I32#)
import U8 as X (U8#)
import Bool as X (Bool#)
import Text.Printf
import F32.Encoding
test (W# -> w) = printf "0x%.8X\n" w

{-print :: F32# -> IO ()-}
{-print f = Prelude.print (GHC.F# f)-}

-- * Constructors
-- | A numeric float encoded as a significand and exponent
pattern Finite :: I32# -> I32# -> F32#
pattern Finite {mantissa,exponent} <- (checkFinite# -> (# mantissa,exponent #) )
  where Finite = encodeFinite

-- ** NaNs
pattern Infinite {negInfinity} <- (checkInfinite -> negInfinity)
pattern Infinity <- (checkInfinity -> (# #)) where Infinity = fromIEEE INFINITY
pattern NegInfinity <- (checkNegInfinity -> (# #)) where NegInfinity = fromIEEE NEG_INFINITY
pattern NaN {nanSign, nanQuiet, nanPayload} <- (checkNaN -> (# nanSign, nanQuiet, nanPayload #))
  where NaN s q p = fromIEEE (encodeNaN s q p)

{-# complete Finite, Infinite, NaN #-}


pattern IEEE {ieeeSign,ieeeExp,ieeeMantissa} <- (decodeIEEE -> (# ieeeSign, ieeeExp, ieeeMantissa #))
  where IEEE s e m = fromIEEE (encode s e m)
decodeIEEE :: F32# -> (# Bool#, U8#, U32# #)
decodeIEEE f = decode (toIEEE f)

checkFinite# :: F32# -> (# I32#, I32# #)
{-# INLINE checkFinite# #-}
checkFinite# f | tagToEnum# (isFinite (toIEEE f)) = decodeFinite# f

-- ** Constants
-- | Largest representable finite float
pattern MaxFinite = 3.40282347e+38#
-- | Smallest positive value so that @1+x@ is representable
pattern Epsilon = 1.19209290e-07#
-- | Smallest representable denormalized (exponent is field is 0, representing -126) float
pattern MinDenormal = 1e-45#
-- | Smallest representable normalized (exponent is @>0@, mantissa is assumed a leading @1@ bit) float
pattern MinNormal = 1.17549435e-38#

foreign import ccall unsafe "PrimFloat.h __int_encodeFloat"
  encodeFinite :: I32# -> I32# -> F32#

-- | Decode mantissa and (undbiased) exponent of a finite float
decodeFinite# :: F32# -> (# I32#, I32# #)
{-# INLINE decodeFinite# #-}
decodeFinite# = decodeFloat_Int# 


-- * math
{-foreign import capi unsafe "math.h log1pf" log1p :: F32# -> F32#-}
{-foreign import capi unsafe "math.h expm1f" expm1 :: F32# -> F32#-}

gt, ge, eq, ne, lt, le :: F32# -> F32# -> Bool#
{-# INLINE gt #-}; {-# INLINE ge #-}; {-# INLINE eq #-}
{-# INLINE ne #-}; {-# INLINE lt #-}; {-# INLINE le #-}
gt = gtFloat#; ge = geFloat#; eq = eqFloat#; ne = neFloat#; lt = ltFloat#; le = leFloat#;

add,sub,mul,div, pow :: F32# -> F32# -> F32#
{-# INLINE add #-};{-# INLINE sub #-};{-# INLINE mul #-}; {-# INLINE div #-}
add = plusFloat#; sub = minusFloat#; mul = timesFloat#; div = divideFloat#; pow = powerFloat#

neg, abs :: F32# -> F32#
{-# INLINE neg #-};{-# INLINE abs #-}
neg = negateFloat#; abs = fabsFloat#

exp, log, sqrt :: F32# -> F32#
{-# INLINE exp #-}; {-# INLINE log #-}; {-# INLINE sqrt #-}
exp = expFloat#; log = logFloat#; sqrt = sqrtFloat#;

sin,asin,sinh :: F32# -> F32#
{-# INLINE sin #-}; {-# INLINE asin #-}; {-# INLINE sinh #-}
sin = sinFloat#; asin = asinFloat#; sinh = sinhFloat#

cos,acos,cosh :: F32# -> F32#
{-# INLINE cos #-}; {-# INLINE acos #-}; {-# INLINE cosh #-}
cos = cosFloat#; acos = acosFloat#; cosh = coshFloat#

tan,atan,tanh :: F32# -> F32#
{-# INLINE tan #-}; {-# INLINE atan #-}; {-# INLINE tanh #-}
tan = tanFloat#; atan = atanFloat#; tanh = tanhFloat#

-- * Utilities for patterns
checkInfinity (toIEEE -> w) | tagToEnum# (isInfinity w) = (# #)
checkNegInfinity (toIEEE -> w) | tagToEnum# (isNegInfinity w) = (# #)
checkInfinite (toIEEE -> w) | tagToEnum# (isInfinite w) = isNegative w
checkNaN (toIEEE -> w) | tagToEnum# (isNaN w) = decodeNaN w
