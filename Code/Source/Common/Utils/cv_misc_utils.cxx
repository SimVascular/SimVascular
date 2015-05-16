/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including 
 * without limitation the rights to use, copy, modify, merge, publish, 
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included 
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "SimVascular.h" 

#include <stdio.h>
#include "cv_misc_utils.h"


// ==========
//   PopArg
// ==========

char *PopArg( int *argc, char **argv[] )
{
  char *arg;

  (*argc)--;
  arg = (*argv)[0];
  (*argv)++;
  return arg;
}


// ------------------
// FindMachineEpsilon
// ------------------
// By epsilon, we're referring to the smallest quantity e which causes
// 1.0 + e to still be > 1.0.  (Note that tests against 0.0 is
// potentially misleading since there may be special hardware for 0.0
// arithmetic.)  More precisely, perhaps, the returned value e will be
// the smallest value s.t. values e' an order of magnitude or more
// smaller than e will not give 1.0 + e' > 1.0.  Also note that
// epsilon is completely different from the smallest double values
// which the machine can represent... instead, epsilon is the smallest
// *interval* which the machine can distinguish.

double FindMachineEpsilon()
{
  double num = 1.0;
  double test = 1.0;

  while ( num + test > num ) {
    test /= 10.0;
  }
  return (test * 10.0);
}


// -----------
// Compute3dks
// -----------
// This function computes the principle curvatures, k1 and k2, given
// quantities for Gaussian curvature Kg and mean curvature Km.  Note
// that while we compute Kg and Km from the level set function, they
// are defined as:
//
//   Kg = k1 * k2        Km = (k1 + k2) / 2
//
// As such, we can solve for the two unknowns k1 and k2.  Note that
// while Km given this definition is ambiguous w.r.t. sign (i.e. a
// direction for the principle curvatures k1 and k2 determines their
// sign), Km as derived from the level set equation embeds information
// about inside vs. outside.  As a result, we can use additional
// information in the sign of Km in selecting the appropriate root of
// the quadratic.
//
// Note that the parameter ks must be able to hold 2 returned values:
//   ks[0] <--> k1
//   ks[1] <--> k2

int Compute3dks( double Kg, double Km, double tol, double ks[] )
{
  double kia, kja;
  double kib, kjb;
  double k1a, k2a;
  double k1b, k2b;
  double tmp;

  tmp = (4 * Km * Km) - (4 * Kg);
  if ( tmp < 0.0 ) {
    return CV_ERROR;
  }
  tmp = sqrt( tmp );

  kia = (2 * Km + tmp) / 2;
  kja = Kg / kia;
  k1a = minimum( kia, kja );
  k2a = maximum( kia, kja );

  kib = (2 * Km - tmp) / 2;
  kjb = Kg / kib;
  k1b = minimum( kib, kjb );
  k2b = maximum( kib, kjb );

  if ( ( (Km > 0.0) && (k2a > 0.0) ) ||
       ( (Km < 0.0) && (k1a < 0.0) ) ||
       ( (Sign( Km, tol ) == 0.0) && (k1a <= 0.0 ) ) ) {
    ks[0] = k1a;
    ks[1] = k2a;
    return CV_OK;
  }

  if ( ( (Km > 0.0) && (k2b > 0.0) ) ||
       ( (Km < 0.0) && (k1b < 0.0) ) ||
       ( (Sign( Km, tol ) == 0.0) && (k1b <= 0.0 ) ) ) {
    ks[0] = k1b;
    ks[1] = k2b;
    return CV_OK;
  }

  return CV_ERROR;
}


// ==============
//   CountLines
// ==============
// For files which have a newline as the last character, the value
// returned is equal to the number of newline characters found in the
// file.  However, if the file does not end with a newline, CountLines
// returns the number of newlines plus one, so that any distinct line
// is counted.

int CountLines( char *filename )
{
  FILE *fp;
  int c, prevC;
  int count = 0;

  prevC = EOF;
  fp = fopen( filename, "r" );
  if (fp == NULL) return -1;
  while ( (c = fgetc(fp)) != EOF ) {
    if (c == '\n') {
      count++;
    }
    prevC = c;
  }
  if (prevC != '\n') {
    count++;
  }
  fclose(fp);
  return count;
}
