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

#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cvMath.h"

#include "cv_math_init.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Prototypes:
#ifdef SV_USE_PYTHON
#include "Python.h"

static PyObject *MathErr;
PyObject *Math_FFTCmd( PyObject *self, PyObject *args );
PyObject *Math_inverseFFTCmd(PyObject *self, PyObject *args  );
PyObject *Math_computeWomersleyCmd( PyObject *self, PyObject *args  );
PyObject *Math_linearInterpCmd( PyObject *self, PyObject *args  );
PyObject *Math_curveLengthCmd( PyObject *self, PyObject *args  );
PyObject *Math_linearInterpolateCurveCmd( PyObject *self, PyObject *args  );
PyObject *Math_fitLeastSquaresCmd( PyObject *self, PyObject *args  );
PyObject *Math_smoothCurveCmd( PyObject *self, PyObject *args  );
PyMODINIT_FUNC
initpyMath(void);

// ---------
// Math_Init
// ---------

int Math_Init( )
{

  Py_Initialize();

  initpyMath();

  return SV_OK;
}

// -----------
// Math_FFTCmd
// -----------

PyObject *Math_FFTCmd(PyObject *self, PyObject *args)
{
  char *usage;

  int nterms = 0;
  int numInterpPoints = 0;

  PyObject *pointsArg;
  
    if (!PyArg_ParseTuple(args,"Oii", &pointsArg, 
      &nterms,&numInterpPoints))
    {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 2 int: pointsArg, nterms, numInterpPoints");
      return NULL;
    }  

  // Do work of command
  if (!PyList_Check(pointsArg)){
    PyErr_SetString( MathErr, "pointsArg not a list");
    return SV_ERROR;
  }
  int nlistpts = PyList_Size(pointsArg);
  if (nlistpts < 0)   return NULL;
  int npts = 0;

  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,2);
  double pt[2];

  for (i = 0; i < nlistpts; i++) {
    PyObject *temp=PyList_GetItem(pointsArg,i);
    if (temp!=NULL)
    {
      for (int j=0;j<2;j++)
      {
        pts[i][j]=PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
    }
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  double **terms = NULL;
  if ((mathobj->FFT(pts, nlistpts, numInterpPoints, nterms, &terms)) == SV_ERROR) {
     PyErr_SetString( MathErr, "error in fft");
     mathobj->deleteArray(pts,nlistpts,2);
     delete mathobj;
     return SV_ERROR;
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(nterms);
  if(pylist!=NULL)
  {
  for (i = 0; i < nterms; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le",terms[i][0],terms[i][1]);
    PyObject *rr=PyString_FromString(r);
    if(!rr)
    {
      Py_DECREF(pylist);
      return NULL;
    }
    PyList_SET_ITEM(pylist, i, rr);
    }
  }  

  // clean up
  mathobj->deleteArray(pts,nlistpts,2);
  mathobj->deleteArray(terms,nterms,2);
  delete mathobj;

  return pylist;
}


// ------------------
// Math_inverseFFTCmd
// ------------------

PyObject *Math_inverseFFTCmd(PyObject *self, PyObject *args)
{
  double t0 = 0;
  double dt = 0;
  double omega = 0;
  int numPts = 0;

  PyObject *termsArg;
  
    if (!PyArg_ParseTuple(args,"Odddi", &termsArg, 
      &t0,&dt,&omega,&numPts))
    {
      PyErr_SetString(MathErr, "Could not import 1 tuple, 3 double and 1 int: termsArg, t0, dt,omega, numPts");
      return NULL;
    }


  // Do work of command
  if (!PyList_Check(termsArg)){
    PyErr_SetString( MathErr, "termsArg is not a list");
    return SV_ERROR;
  }
  int nlistterms = PyList_Size(termsArg);
  if (nlistterms < 0)   return NULL;

  int numTerms = 0;

  int i;
  cvMath *mathobj = new cvMath();
  double **terms = mathobj->createArray(nlistterms,2);
  double term[2];
  int nt = 0;

  for (i = 0; i < nlistterms; i++) {

    PyObject *temp=PyList_GetItem(termsArg,i);
    if (temp!=NULL)
    {
      for (int j=0;j<2;j++)
      {
        terms[i][j]=PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
    }

  }

  //for (i = 0; i < nlistterms; i++) {
  //     fprintf(stdout,"Term %i:  %lf %lf\n",i,terms[i][0],terms[i][1]);
  //}

  double **pts = NULL;
  if ( (mathobj->inverseFFT(terms, nlistterms, t0, dt, omega, numPts, &pts)) == SV_ERROR) {
     PyErr_SetString( MathErr, "error in inverse fft" );
     mathobj->deleteArray(terms,nlistterms,2);
     delete mathobj;
     return SV_ERROR;
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numPts);
  if(pylist!=NULL)
  {
  for (i = 0; i < numPts; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le",pts[i][0],pts[i][1]);
    PyObject *rr=PyString_FromString(r);
    if(!rr)
    {
      Py_DECREF(pylist);
      return NULL;
    }
    PyList_SET_ITEM(pylist, i, rr);
    }
  }  

  // clean up
  mathobj->deleteArray(terms,nlistterms,2);
  mathobj->deleteArray(pts,numPts,2);
  delete mathobj;

  return pylist;
}


// ------------------------
// Math_computeWomersleyCmd
// ------------------------
PyObject *Math_computeWomersleyCmd(PyObject *self, PyObject *args)
{
  double time = 0;
  double viscosity = 0;
  double omega = 0;
  double density = 0;
  double radmax = 0;
  double radius = 0;

  PyObject *termsArg;
  
    if (!PyArg_ParseTuple(args,"Odddddd", PyList_Type, &termsArg, 
      &time,&viscosity,&omega,&density,&radmax,&radius))
    {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 6 double: termsArg,time,viscosity,omega,density,radmax,radius");
      return NULL;
    }


  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );

  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_sz, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command
  if (!PyList_Check(termsArg)){
    PyErr_SetString( MathErr, "termsArg not a list");
    return SV_ERROR;
  }
  int nlistterms = PyList_Size(termsArg);
  if (nlistterms < 0)   return NULL;
  int numTerms = 0;

  int i;
  cvMath *mathobj = new cvMath();
  double **terms = mathobj->createArray(nlistterms,2);
  int nt = 0;

  for (i = 0; i < nlistterms; i++) {
    PyObject *temp=PyList_GetItem(termsArg,i);
    if (temp!=NULL)
    {
      for (int j=0;j<2;j++)
      {
        terms[i][j]=PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
    }
  }

  //for (i = 0; i < nlistterms; i++) {
  //     fprintf(stdout,"Term %i:  %lf %lf\n",i,terms[i][0],terms[i][1]);
  //}

  double velocity = 0;
  if ((mathobj->compute_v_womersley(terms, nlistterms, viscosity, density,
                     omega, radmax, radius, time, &velocity)) == SV_ERROR) {
     PyErr_SetString( MathErr, "error in calculate worm" );
     mathobj->deleteArray(terms,nlistterms,2);
     delete mathobj;
     return SV_ERROR;
  }

  // create result string
  char r[2048];
  r[0] = '\0';
  sprintf(r,"%.6le",velocity);

  // clean up
  mathobj->deleteArray(terms,nlistterms,2);
  delete mathobj;

  return Py_BuildValue("s",r);
}

PyObject *Math_linearInterpCmd(PyObject *self, PyObject *args)
{
  int numInterpPoints = 0;

  PyObject *pointsArg;
  
  if (!PyArg_ParseTuple(args,"Oi", &pointsArg, 
      &numInterpPoints))
  {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 1 int: termsArg,numInterpPoints");
      return NULL;
  }  
  

  // Do work of command

  if (!PyList_Check(pointsArg)){
    PyErr_SetString( MathErr, "pointsArg not a list");
    return SV_ERROR;
  }
  int nlistpts = PyList_Size(pointsArg);
  if (nlistpts < 0)   return NULL;
  int npts = 0;


  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,2);


  for (i = 0; i < nlistpts; i++) {

    PyObject *temp=PyList_GetItem(pointsArg,i);
    if (temp!=NULL)
    {
      for (int j=0;j<2;j++)
      {
        pts[i][j]=PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
    }
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  // here we calculate dt so that our time series will go from
  // 0 to T.
  double t0 = pts[0][0];
  double dt = (pts[nlistpts-1][0]-t0)/(numInterpPoints-1);
  double **outPts = NULL;

  if ((mathobj->linearInterpolate(pts, nlistpts, t0, dt, numInterpPoints, &outPts)) == SV_ERROR) {
     PyErr_SetString( MathErr, "error in linear interpolation");
     mathobj->deleteArray(pts,nlistpts,2);
     delete mathobj;
     return SV_ERROR;
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numInterpPoints);
  if(pylist!=NULL)
  {
  for (i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le",outPts[i][0],outPts[i][1]);
    PyObject *rr=PyString_FromString(r);
    if(!rr)
    {
      Py_DECREF(pylist);
      return NULL;
    }
    PyList_SET_ITEM(pylist, i, rr);
    }
  }

  // clean up
  mathobj->deleteArray(pts,nlistpts,2);
  mathobj->deleteArray(outPts,numInterpPoints,2);
  delete mathobj;

  return pylist;
}

PyObject *Math_curveLengthCmd(PyObject *self, PyObject *args)
{
  int closed = 0;

  PyObject *pointsArg;
  if (!PyArg_ParseTuple(args,"Oi", PyList_Type, &pointsArg, 
      &closed))
  {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 1 int: termsArg,closed");
      return NULL;
  }  

  // Do work of command
  if (!PyList_Check(pointsArg)){
    PyErr_SetString( MathErr, "pointsArg not a list");
    return SV_ERROR;
  }
  int nlistpts = PyList_Size(pointsArg);
  if (nlistpts < 0)   return NULL;
  int npts = 0;

  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,3);
  double pt[3];

  for (i = 0; i < nlistpts; i++) {
    PyObject *temp=PyList_GetItem(pointsArg,i);
    if (temp!=NULL)
    {
      for (int j=0;j<3;j++)
      {
        pts[i][j]=PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
    }
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  double length = 0;
  if ((mathobj->curveLength(pts, nlistpts, closed, &length)) == SV_ERROR) {
     PyErr_SetString( MathErr, "error finding curve length" );
     mathobj->deleteArray(pts,nlistpts,3);
     delete mathobj;
     return SV_ERROR;
  }

  // create result string
  char r[2048];
  r[0] = '\0';
  sprintf(r,"%.6le",length);
  //Tcl_AppendElement(interp, r);

  // clean up
  mathobj->deleteArray(pts,nlistpts,3);
  delete mathobj;

  return Py_BuildValue("s",r);
}

PyObject *Math_linearInterpolateCurveCmd(PyObject *self, PyObject *args)
{
  int numInterpPoints = 0;
  int closed = 0;

  PyObject *pointsArg;
  if (!PyArg_ParseTuple(args,"Oii", PyList_Type, &pointsArg, 
      &closed,&numInterpPoints))
  {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 2 int: termsArg,closed,&numInterpPoints");
      return NULL;
  }  
  
  // Do work of command
  if (!PyList_Check(pointsArg)){
    PyErr_SetString( MathErr, "pointsArg not a list");
    return SV_ERROR;
  }
  int nlistpts = PyList_Size(pointsArg);
  if (nlistpts < 0)   return NULL;
  int npts = 0;

  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,3);
  double pt[3];

  for (i = 0; i < nlistpts; i++) {
    PyObject *temp=PyList_GetItem(pointsArg,i);
    if (temp!=NULL)
    {
      for (int j=0;j<3;j++)
      {
        pts[i][j]=PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
    }
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  double **outPts = NULL;
  if ((mathobj->linearInterpolateCurve(pts, nlistpts, closed, numInterpPoints, &outPts)) == SV_ERROR) {
     PyErr_SetString( MathErr, "error in linear interpolation" );
     mathobj->deleteArray(pts,nlistpts,3);
     delete mathobj;
     return SV_ERROR;
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numInterpPoints);
  if(pylist!=NULL)
  {
  for (i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le %.6le",outPts[i][0],outPts[i][1],outPts[i][2]);
    PyObject *rr=PyString_FromString(r);
    if(!rr)
    {
      Py_DECREF(pylist);
      return NULL;
    }
    PyList_SET_ITEM(pylist, i, rr);
    }
  }  
  

  // clean up
  mathobj->deleteArray(pts,nlistpts,3);
  mathobj->deleteArray(outPts,numInterpPoints,3);
  delete mathobj;

  return pylist;
}


// -----------------------
// Math_fitLeastSquaresCmd
// -----------------------
PyObject *Math_fitLeastSquaresCmd(PyObject *self, PyObject *args)
{
  int xOrder = 0;
  int yOrder = 0;

  PyObject *xtermsArg;
  PyObject *ytermsArg;

  if (!PyArg_ParseTuple(args,"OOii", PyList_Type, &xtermsArg, 
                        PyList_Type, &ytermsArg,
                        &xOrder,&yOrder))
  {
      PyErr_SetString(MathErr, "Could not import 2 tuple and 2 int:xtermsArg, termsArg,xOrder,yOrder");
      return NULL;
  }  


  // Do work of command
  if (!(PyList_Check(xtermsArg)||PyList_Check(ytermsArg))){
    PyErr_SetString( MathErr, "xtermsArg or ytermsArg is not a list");
    return SV_ERROR;
  }
  int numberOfSamples = PyList_Size(xtermsArg);
  int numberOfSamplesY = PyList_Size(ytermsArg);
  if (numberOfSamples < 0)   return NULL;

  if (numberOfSamples!= numberOfSamplesY) {

      PyErr_SetString(MathErr,"ERROR:  X and Y must have the same number of samples\n");
  }

  int numTerms = 0;

  int i,j;
  cvMath *mathobj = new cvMath();

  double **xt = mathobj->createArray(numberOfSamples,xOrder);
  for (i = 0; i < numberOfSamples; i++) {
    PyObject *temp=PyList_GetItem(xtermsArg,i);
    if (temp!=NULL)
    {
      for (int j=0;j<3;j++)
      {
        xt[i][j]=PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
    }
  }

  double **yt = mathobj->createArray(numberOfSamples,yOrder);
  for (i = 0; i < numberOfSamples; i++) {
    PyObject *temp=PyList_GetItem(ytermsArg,i);
    if (temp!=NULL)
    {
      for (j=0;j<3;j++)
      {
        yt[i][j]=PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
    }
  }
  // debug print
  for (int k = 0; k < numberOfSamples; k++) {
      for (i = 0; i < xOrder; i++) {
       fprintf(stdout,"%lf ",xt[k][i]);
      }
      fprintf(stdout, " M : ");
      for (j = 0; j < yOrder; j++) {
          fprintf(stdout,"%lf ",yt[k][j]);
      }
      fprintf(stdout,"\n");
  }

  double **mt = mathobj->createArray(xOrder,yOrder);
  if ( (mathobj->fitLeastSquares(numberOfSamples,xt,xOrder,yt,yOrder,mt)) == SV_ERROR) {
     PyErr_SetString( MathErr, "error in least squares fit" );
      mathobj->deleteArray(xt,numberOfSamples,xOrder);
      mathobj->deleteArray(yt,numberOfSamples,yOrder);
      delete mathobj;
     return SV_ERROR;
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(xOrder);
  if(pylist!=NULL)
  {
  for (i = 0; i < xOrder; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le ",mt[i][j]);
    PyObject *rr=PyString_FromString(r);
    if(!rr)
    {
      Py_DECREF(pylist);
      return NULL;
    }
    PyList_SET_ITEM(pylist, i, rr);
    }
  }  

  // clean up
  mathobj->deleteArray(xt,numberOfSamples,xOrder);
  mathobj->deleteArray(yt,numberOfSamples,yOrder);
  mathobj->deleteArray(mt,xOrder,yOrder);
  delete mathobj;

  return pylist;
}


PyObject *Math_smoothCurveCmd(PyObject *self, PyObject *args)
{
  int numInterpPoints = 0;
  int closed = 0;
  int numModes = 0;

  PyObject *pointsArg;
  if (!PyArg_ParseTuple(args,"Oiii", PyList_Type, &pointsArg, 
      &closed,&numModes,&numInterpPoints))
  {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 3 int: termsArg,closed,numModes,numInterpPoints");
      return NULL;
  }  


  // Do work of command
  if (!PyList_Check(pointsArg)){
    PyErr_SetString( MathErr, "pointsArg is not a list");
    return SV_ERROR;
  }
  int nlistpts = PyList_Size(pointsArg);
  if (nlistpts < 0)   return NULL;
  int npts = 0;

  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,3);

  for (i = 0; i < nlistpts; i++) {
    PyObject *temp=PyList_GetItem(pointsArg,i);
    if (temp!=NULL)
    {
      for (int j=0;j<3;j++)
      {
        pts[i][j] = PyFloat_AsDouble(PyList_GetItem(temp,j));
      }
    }
  }

  double **outPts = NULL;
  if ((mathobj->smoothCurve(pts, nlistpts, closed, numModes, numInterpPoints, &outPts)) == SV_ERROR) {
     PyErr_SetString( MathErr, "error in smoothing curve");
     mathobj->deleteArray(pts,nlistpts,3);
     delete mathobj;
     return SV_ERROR;
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numInterpPoints);
  if(pylist!=NULL)
  {
  for (i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le %.6le",outPts[i][0],outPts[i][1],outPts[i][2]);
    PyObject *rr=PyString_FromString(r);
    if(!rr)
    {
      Py_DECREF(pylist);
      return NULL;
    }
    PyList_SET_ITEM(pylist, i, rr);
    }
  }  
  // clean up
  mathobj->deleteArray(pts,nlistpts,3);
  mathobj->deleteArray(outPts,numInterpPoints,3);
  delete mathobj;

  return pylist;
}
//All functions listed and initiated as pyMath_methods declared here
// --------------------
// pyImage_methods
// --------------------
PyMethodDef pyMath_methods[] = {
  {"math_FFT", Math_FFTCmd, METH_VARARGS,NULL},
  {"math_inverseFFT", Math_inverseFFTCmd, METH_VARARGS,NULL},
  {"math_computeWomersley", Math_computeWomersleyCmd, METH_VARARGS,NULL},
  {"math_linearInterp", Math_linearInterpCmd, METH_VARARGS,NULL},
  {"math_curveLength", Math_curveLengthCmd, METH_VARARGS,NULL},
  {"math_linearInterpCurve", Math_linearInterpolateCurveCmd, METH_VARARGS,NULL},
  {"math_fitLeastSquares", Math_fitLeastSquaresCmd, METH_VARARGS,NULL},
  {"math_smoothCurve", Math_smoothCurveCmd, METH_VARARGS,NULL},
  {NULL, NULL,0,NULL},
  };


// --------------------
// initpyImage
// --------------------
PyMODINIT_FUNC
initpyMath(void)
{
PyObject *pyMth;

pyMth = Py_InitModule("pyMath",pyMath_methods);

MathErr = PyErr_NewException("pyMath.error",NULL,NULL);
Py_INCREF(MathErr);
PyModule_AddObject(pyMth,"error",MathErr);
}
#endif