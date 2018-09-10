#include "HsFFI.h"
#include "Rts.h" // XXX wrong (for IEEE_FLOATING_POINT and WORDS_BIGENDIAN)

#define IEEE_FLOATING_POINT 1

union f32_t { //little endian
  float f;
  uint32_t w;
  struct {  
    uint32_t m : 23; // mantissa/significand. Implicit leading bit unless e is 0
    uint32_t e : 8; // exponent biased by 127
    bool s : 1; // sign bit, 1 if negative
  } ieee;
  struct {
    uint32_t p : 22; //payload, arbitrary bits. All zero for infinity
    bool q : 1; //quiet bit, 1 if quiet
    uint32_t e : 8;
    bool s : 1;
  } nan;
};

HsWord toWord(HsFloat f) {union f32_t u = {f}; return u.w;}
HsFloat fromWord(HsWord w) {union f32_t u = { .w = w }; return u.f;}

HsFloat encodeNaN(uint32_t p,bool q, uint32_t e, bool s) {
  union f32_t u = {.nan = {.p = p, .q = q, .e = 0xFF, .s = s}};
  return u.f;
}

HsFloat encodeIEEE(uint32_t m, uint32_t e, bool s) {
  union f32_t u = {.ieee = {.m = m, .e = e, .s = s}};
  return u.f;
}

uint32_t getPayload(HsFloat f) {union f32_t u = {f}; return u.nan.p;}
bool getQuiet(HsFloat f) {union f32_t u = {f}; return u.nan.q;}

uint32_t getExponent(HsFloat f) {union f32_t u = {f}; return u.ieee.e;}
uint32_t getMantissa(HsFloat f) {union f32_t u = {f}; return u.ieee.m;}
bool getSign(HsFloat f) {union f32_t u = {f}; return u.ieee.s;}

bool isFinite(HsFloat f) {union f32_t u = {f}; return (u.ieee.e != 0xFF);}
bool isInfinite(HsFloat f) {union f32_t u = {f}; return (u.ieee.e == 0xFF && u.ieee.m == 0);}
bool isDenormalized(HsFloat f) {union f32_t u = {f}; return (u.ieee.e == 0 && u.ieee.m != 0);}
bool isNegZero(HsFloat f) {union f32_t u = {f}; return (u.ieee.s && u.ieee.e == 0 && u.ieee.m == 0);}
bool isNaN(HsFloat f) {union f32_t u = {f}; return (u.ieee.e == 0xFF && u.ieee.m != 0);}

/*HsFloat round(HsFloat f) {*/
  /*union f32_t = {f};*/
  /*if (u.ieee.e > 149) [> 22 + 127 <] {return u.f;}*/
  /*if (u.ieee.e < 126) [> -1 + 127, abs(f) < 0.5 <] {return 0.0;}*/
  /*unsigned int half = 1 << (149 - u.ieee.e); // bit for 0.5*/
  /*unsigned int mask = 2*half - 1; // fraction bits*/
  /*unsigned int mant = u.ieee.m | 0x800000; // add hidden bit*/
  /*unsigned int frac = mant & mask;*/
  /*mant ^= frac; // truncate mantissa*/
  /*if ((frac < half) || ((frac == half) && ((mant & (2*half)) == 0))) {*/
    /*if (mant == 0) {return 0.0;} // f == +/- 0.5*/
    /*else {u.ieee.m = mant ^ 0x800000; return u.f;} [> remove hidden bit and set mantissa <] } */
  /*else { // round away from zero, increment mantissa*/
    /*mant += 2*half;*/
    /*if (mant == 0x1000000) [> next power of 2 <] {u.ieee.m = 0; u.ieee.e += 1; return u.f;}*/
    /*else {u.ieee.m = mant ^ 0x800000; return u.f;} [> remove hidden bit and set mantissa <] }*/


