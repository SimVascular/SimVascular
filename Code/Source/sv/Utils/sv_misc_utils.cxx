/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "SimVascular.h"

#include <stdio.h>

#include "sv_misc_utils.h"

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
    return SV_ERROR;
  }
  tmp = sqrt( tmp );

  kia = (2 * Km + tmp) / 2;
  kja = Kg / kia;
  k1a = svminimum( kia, kja );
  k2a = svmaximum( kia, kja );

  kib = (2 * Km - tmp) / 2;
  kjb = Kg / kib;
  k1b = svminimum( kib, kjb );
  k2b = svmaximum( kib, kjb );

  if ( ( (Km > 0.0) && (k2a > 0.0) ) ||
       ( (Km < 0.0) && (k1a < 0.0) ) ||
       ( (cvSign( Km, tol ) == 0.0) && (k1a <= 0.0 ) ) ) {
    ks[0] = k1a;
    ks[1] = k2a;
    return SV_OK;
  }

  if ( ( (Km > 0.0) && (k2b > 0.0) ) ||
       ( (Km < 0.0) && (k1b < 0.0) ) ||
       ( (cvSign( Km, tol ) == 0.0) && (k1b <= 0.0 ) ) ) {
    ks[0] = k1b;
    ks[1] = k2b;
    return SV_OK;
  }

  return SV_ERROR;
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
