#include <float.h>
#include <math.h>
#include <stdint.h>


#define REAL             float
#define REAL_ABS         fabsf

#define REAL_MIN_NORMAL  FLT_MIN
#define REAL_EPSILON     FLT_EPSILON
#define REAL_MAX         FLT_MAX
#define REAL_MANT_DIG    FLT_MANT_DIG

#define FEQREL           feqrelf

#define MANTISSA_MASK 0x7FFFFF
#define EXP_MASK  0x7F800000
#define EXP_SHIFT 
#define QUIET_MASK  0x400000
#define QUITE_SHIFT 22
#define PAYLOAD_MASK  0x3FFFFF
#define SIGN_MASK  0x80000000


#include "feqrel_source.c"

union float_t {
    float f;
    uint32_t w;
};

uint32_t ieeeBits (float x) { union float_t ux = {x}; return ux.w; }
float ieeeEncode32 (uint32_t x) { union float_t ux = {x}; return ux.f; }

uint32_t getMantissa (float x) { union float_t ux = {x}; return ux.w & MANTISSA_MASK; }
uint32_t getExponent (float x) { union float_t ux = {x}; return ux.parts.exponent; }
uint32_t getSign (float x) { union float_t ux = {x}; return ux.parts.sign; }


int
identicalf (float x, float y)
{
    union float_t ux = { x };
    union float_t uy = { y };
    return ux.w == uy.w;
}

float
copysignf (float x, float y)
{
    union float_t ux = { x };
    union float_t uy = { y };
    union float_t uz;
    uint32_t val  = ux.w & 0x7FFFFFFF;
    uint32_t sign = uy.w & 0x80000000;
    uz.w = sign | val;
    return uz.f;
}

/* ported from tango/math/IEEE.d nextupf */
float
ieeesuccf (float x)
{
    union float_t ps = { x };

    if ((ps.w & 0x7F800000) == 0x7F800000) {
        /* First, deal with NANs and infinity */
        if (x == -INFINITY) return -REAL_MAX;
        return x; /* +INF and NAN are unchanged. */
    }
    if (ps.w & 0x80000000)  { /* Negative number */
        if (ps.w == 0x80000000) { /* it was negative zero */
            ps.w = 0x00000001; /* change to smallest subnormal */
            return ps.f;
        }
        --ps.w;
    } else { /* Positive number */
        ++ps.w;
    }
    return ps.f;
}

/* ported from tango/math/IEEE.d nextdownf */
float
ieeepredf (float x)
{
    return -ieeesuccf(-x);
}

/* ported from tango/math/IEEE.d */
float
ieeemeanf (float x, float y)
{
    if (!((x>=0 && y>=0) || (x<=0 && y<=0))) return NAN;
    
    union float_t ul;
    union float_t xl = { x };
    union float_t yl = { y };
    uint32_t m = ((xl.w & 0x7FFFFFFF) + (yl.w & 0x7FFFFFFF)) >> 1;
    m |= (xl.w & 0x80000000);
    ul.w = m;
    
    return ul.f;
}

float
mknanf (uint32_t payload)
{
    union float_t ux = { NAN };
    
    /* get sign, exponent, and quiet bit from NAN */    
    ux.w &= 0xFFC00000; 
    
    /* ignore sign, exponent, and quiet bit in payload */
    payload &= 0x003FFFFF;
    ux.w |= payload;

    return ux.f;
}

uint32_t
getnanf (float x)
{
    union float_t payload = { x };
    
    /* clear sign, exponent, and quiet bit */
    payload.w &= 0x003FFFFF;
    return payload.w;
}
