/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
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
 *
 *=========================================================================*/
 
#ifndef __CVSOLID_INIT_H
#define __CVSOLID_INIT_H

#include "SimVascular.h"
#include "tcl.h"
#ifdef USE_PYTHON
#include "Python.h"
#endif

extern "C" CV_DLL_EXPORT int Solid_Init( Tcl_Interp *interp );

//Solid commands that need to be recognized in functions outside the solid model module
int Solid_ObjectCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

void DeleteSolid( ClientData clientData );

#ifdef USE_PYTHON

double *getArrayFromDoubleList(PyObject* listObj,int &len);

double **getArrayFromDoubleList2D(PyObject* listObj,int &lenx,int &leny);

PyObject* importList1D(PyObject* self, PyObject* args);

PyObject* importList2D(PyObject* self, PyObject* args);

#endif

#endif // __CVSOLID_INIT_H
