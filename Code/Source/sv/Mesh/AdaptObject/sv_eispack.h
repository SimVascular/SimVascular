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
#include "svAdaptorExports.h" // For exports

SV_EXPORT_ADAPTOR int bakvec ( int n, double t[], double e[], int m, double z[] );
SV_EXPORT_ADAPTOR void cbabk2 ( int n, int low, int igh, double scale[], int m, double zr[],
  double zi[] );
SV_EXPORT_ADAPTOR void bandr ( int n, int mb, double a[], double d[], double e[], double e2[],
  int matz, double z[] );
SV_EXPORT_ADAPTOR void csroot ( double xr, double xi, double &yr, double &yi );
SV_EXPORT_ADAPTOR int i4_max ( int i1, int i2 );
SV_EXPORT_ADAPTOR int i4_min ( int i1, int i2 );
SV_EXPORT_ADAPTOR double pythag ( double a, double b );
SV_EXPORT_ADAPTOR double r8_abs ( double x );
SV_EXPORT_ADAPTOR double r8_epsilon ( );
SV_EXPORT_ADAPTOR double r8_max ( double x, double y );
SV_EXPORT_ADAPTOR double r8_min ( double x, double y );
SV_EXPORT_ADAPTOR double r8_sign ( double x );
SV_EXPORT_ADAPTOR void r8mat_identity  ( int n, double a[] );
SV_EXPORT_ADAPTOR double *r8mat_mm_new ( int n1, int n2, int n3, double a[], double b[] );
SV_EXPORT_ADAPTOR void r8mat_print ( int m, int n, double a[], std::string title );
SV_EXPORT_ADAPTOR void r8mat_print_some ( int m, int n, double a[], int ilo, int jlo, int ihi,
  int jhi, std::string title );
SV_EXPORT_ADAPTOR double *r8mat_uniform_01_new ( int m, int n, int &seed );
SV_EXPORT_ADAPTOR void r8vec_print ( int n, double a[], std::string title );
SV_EXPORT_ADAPTOR int rs ( int n, double a[], double w[], int matz, double z[] );
SV_EXPORT_ADAPTOR int rsb ( int n, int mb, double a[], double w[], int matz, double z[] );
SV_EXPORT_ADAPTOR void timestamp ( );
SV_EXPORT_ADAPTOR int tql2 ( int n, double d[], double e[], double z[] );
SV_EXPORT_ADAPTOR int tqlrat ( int n, double w[], double fv2[] );
SV_EXPORT_ADAPTOR void tred1 ( int n, double a[], double w[], double fv1[], double fv2[] );
SV_EXPORT_ADAPTOR void tred2 ( int n, double a[], double d[], double e[], double z[] );

