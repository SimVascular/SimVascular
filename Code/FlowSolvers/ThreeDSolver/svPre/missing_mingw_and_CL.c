/* ===-- powidf2.cpp - Implement __powidf2 ---------------------------------===
 *
 *                     The LLVM Compiler Infrastructure
 *
 * This file is dual licensed under the MIT and the University of Illinois Open
 * Source Licenses. See LICENSE.TXT for details.
 *
 * ===----------------------------------------------------------------------===
 *
 * This file implements __powidf2 for the compiler_rt library.
 *
 * ===----------------------------------------------------------------------===
 */

typedef      int si_int;

/* Returns: a ^ b */

double
__powidf2(double a, si_int b)
{
    const int recip = b < 0;
    double r = 1;
    while (1)
    {
        if (b & 1)
            r *= a;
        b /= 2;
        if (b == 0)
            break;
        a *= a;
    }
    return recip ? 1/r : r;
}

/*===-- powisf2.cpp - Implement __powisf2 ---------------------------------===
 *
 *                    The LLVM Compiler Infrastructure
 *
 * This file is dual licensed under the MIT and the University of Illinois Open
 * Source Licenses. See LICENSE.TXT for details.
 *
 * ===----------------------------------------------------------------------===
 *
 * This file implements __powisf2 for the compiler_rt library.
 *
 * ===----------------------------------------------------------------------===
 */

typedef      int si_int;

/* Returns: a ^ b */

float
__powisf2(float a, si_int b)
{
    const int recip = b < 0;
    float r = 1;
    while (1)
    {
        if (b & 1)
            r *= a;
        b /= 2;
        if (b == 0)
            break;
        a *= a;
    }
    return recip ? 1/r : r;
}
/* hack routine, just guessing at args */
#include <math.h>
float cabsf(float* a, float *b) {
  return sqrt((*a)*(*a)+(*b)*(*b));
}
