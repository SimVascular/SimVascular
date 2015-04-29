/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * RPI, Charles Taylor, Ken Jansen, Nathan Wilson, Ken Wang.
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
 *
 *=========================================================================*/

#ifndef _ADAPTHELPERS_H
#define _ADAPTHELPERS_H

// standard includes
#include <stdlib.h>
#include <stdio.h>
#include <algorithm>
#include <math.h>
#include <iostream>
#include <string>
#include <fstream>
#include <time.h>
#include <vector>
#include <map>

#ifndef WIN32
#include <strings.h>
#include <unistd.h>
#endif
#ifdef WIN32
  #include <direct.h>
  #define chdir _chdir
  #define M_PI 3.14159265353846f
  void  bzero(void* ptr, size_t sz);
#endif

#define ABS(x) ((x) < 0 ? -(x) : (x))
#define MAX(x,y) ((x)<(y) ? (y) : (x))

#include "cvSolverIO.h"

#include "SimVascular.h"

// to read parameters from a phasta file (filename)
// parameters correspond to nshg & nvar, i.e., size of field-array
// these parameters are used as reference values 
// (sometimes needed before reading the field-array)
void readParametersFromFile (char *filename, char *fieldName,
		       int &nshg, int &numVars);

// to read array from a phasta file (filename)
// memory is allocated HERE for 'valueArray'
// `fieldName' tells which block to read like solution, error etc.
void readArrayFromFile ( char *filename, char *fieldName,
		  double *&valueArray);

// to write array to a phasta file (filename)
// NOTE: array should be transposed!!!
// `fieldName' tells in which block to write like solution, error etc.
// `outputFormat' tells in which format to write, i.e., binary/ascii
// `mode' : "write", "appeand" etc.
void writeArrayToFile ( char *filename, char *fieldName,
		  char *outputFormat, char *mode,
		  int nshg, int numVars,
		  int stepNumber, double *valueArray); 

#endif
