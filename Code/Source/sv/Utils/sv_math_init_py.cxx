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
#include "Python.h"
#include "SimVascular.h"

#include <stdio.h>
#include <iostream>

#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_Math.h"

#include "sv_math_init_py.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif
// Prototypes:


static PyObject *MathErr;
static PyObject *pyMath_FFTCmd( PyObject *self, PyObject *args );
PyObject *pyMath_inverseFFTCmd(PyObject *self, PyObject *args  );
PyObject *pyMath_computeWomersleyCmd( PyObject *self, PyObject *args  );
PyObject *pyMath_linearInterpCmd( PyObject *self, PyObject *args  );
PyObject *pyMath_curveLengthCmd( PyObject *self, PyObject *args  );
PyObject *pyMath_linearInterpolateCurveCmd( PyObject *self, PyObject *args  );
PyObject *pyMath_fitLeastSquaresCmd( PyObject *self, PyObject *args  );
PyObject *pyMath_smoothCurveCmd( PyObject *self, PyObject *args  );
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC
initpyMath(void);
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyMath(void);
#endif


// ---------
// Math_Init
// ---------

int Math_pyInit( )
{

#if PYTHON_MAJOR_VERSION == 2
  initpyMath();
#elif PYTHON_MAJOR_VERSION == 3
  PyInit_pyMath();
#endif
  return SV_OK;
}

// -----------
// Math_FFTCmd
// -----------

static PyObject *pyMath_FFTCmd(PyObject *self, PyObject *args)
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
        //printf("%f\n",pts[i][j]);
      }
    }
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  double **terms = NULL;
  //printf("%d,%d,%d,%d\n",nlistpts,numInterpPoints,nterms,&terms);
  if ((mathobj->FFT(pts, nlistpts, numInterpPoints, nterms, &terms)) == SV_ERROR) {
     PyErr_SetString( MathErr, "error in fft");
     mathobj->deleteArray(pts,nlistpts,2);
     delete mathobj;
     
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(nterms);
  if(pylist!=NULL)
  {
  for (i = 0; i < nterms; i++) {
      PyObject* rr = PyList_New(2);
      PyList_SetItem(rr,0,PyFloat_FromDouble(terms[i][0]));
      PyList_SetItem(rr,1,PyFloat_FromDouble(terms[i][1]));
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

PyObject *pyMath_inverseFFTCmd(PyObject *self, PyObject *args)
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
     
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numPts);
  if(pylist!=NULL)
  {
  for (i = 0; i < numPts; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le",pts[i][0],pts[i][1]);
    PyObject *rr=PyBytes_FromString(r);
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
PyObject *pyMath_computeWomersleyCmd(PyObject *self, PyObject *args)
{
  double time = 0;
  double viscosity = 0;
  double omega = 0;
  double density = 0;
  double radmax = 0;
  double radius = 0;

  PyObject *termsArg;

    if (!PyArg_ParseTuple(args,"Odddddd", &termsArg,
      &time,&viscosity,&omega,&density,&radmax,&radius))
    {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 6 double: termsArg,time,viscosity,omega,density,radmax,radius");
      return NULL;
    }

  // Do work of command
  if (!PyList_Check(termsArg)){
    PyErr_SetString( MathErr, "termsArg not a list");
    
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
     
  }

  // clean up
  mathobj->deleteArray(terms,nlistterms,2);
  delete mathobj;

  return Py_BuildValue("d",velocity);
}

PyObject *pyMath_linearInterpCmd(PyObject *self, PyObject *args)
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
     
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numInterpPoints);
  if(pylist!=NULL)
  {
  for (i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le",outPts[i][0],outPts[i][1]);
    PyObject *rr=PyBytes_FromString(r);
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

PyObject *pyMath_curveLengthCmd(PyObject *self, PyObject *args)
{
  int closed = 0;

  PyObject *pointsArg;
  if (!PyArg_ParseTuple(args,"Oi", &pointsArg,
      &closed))
  {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 1 int: termsArg,closed");
      return NULL;
  }

  // Do work of command
  if (!PyList_Check(pointsArg)){
    PyErr_SetString( MathErr, "pointsArg not a list");
    
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
     
  }

  // clean up
  mathobj->deleteArray(pts,nlistpts,3);
  delete mathobj;

  return Py_BuildValue("d",length);
}

PyObject *pyMath_linearInterpolateCurveCmd(PyObject *self, PyObject *args)
{
  int numInterpPoints = 0;
  int closed = 0;

  PyObject *pointsArg;
  if (!PyArg_ParseTuple(args,"Oii", &pointsArg,
      &closed,&numInterpPoints))
  {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 2 int: termsArg,closed,&numInterpPoints");
      return NULL;
  }

  // Do work of command
  if (!PyList_Check(pointsArg)){
    PyErr_SetString( MathErr, "pointsArg not a list");
    
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
     
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numInterpPoints);
  if(pylist!=NULL)
  {
  for (i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le %.6le",outPts[i][0],outPts[i][1],outPts[i][2]);
    PyObject *rr=PyBytes_FromString(r);
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
PyObject *pyMath_fitLeastSquaresCmd(PyObject *self, PyObject *args)
{
  int xOrder = 0;
  int yOrder = 0;

  PyObject *xtermsArg;
  PyObject *ytermsArg;

  if (!PyArg_ParseTuple(args,"OOii",  &xtermsArg,
                         &ytermsArg,
                        &xOrder,&yOrder))
  {
      PyErr_SetString(MathErr, "Could not import 2 tuple and 2 int:xtermsArg, termsArg,xOrder,yOrder");
      return NULL;
  }


  // Do work of command
  if (!(PyList_Check(xtermsArg)||PyList_Check(ytermsArg))){
    PyErr_SetString( MathErr, "xtermsArg or ytermsArg is not a list");
    
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
  PyObject *temp;
  double **xt = mathobj->createArray(numberOfSamples,xOrder);
  for (i = 0; i < numberOfSamples; i++) {
    temp=PyList_GetItem(xtermsArg,i);
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
    temp=PyList_GetItem(ytermsArg,i);
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
      delete temp;
      
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(xOrder);
  if(pylist!=NULL)
  {
  for (i = 0; i < xOrder; i++) {
    r[0] = '\0';
    for (j = 0; j < yOrder; j++) {
    sprintf(r,"%.6le ",mt[i][j]);
    PyObject *rr=PyBytes_FromString(r);
    if(!rr)
    {
      Py_DECREF(pylist);
      return NULL;
    }
    PyList_SET_ITEM(pylist, i, rr);
  }
    }
  }

  // clean up
  mathobj->deleteArray(xt,numberOfSamples,xOrder);
  mathobj->deleteArray(yt,numberOfSamples,yOrder);
  mathobj->deleteArray(mt,xOrder,yOrder);
  delete mathobj;

  return pylist;
}


PyObject *pyMath_smoothCurveCmd(PyObject *self, PyObject *args)
{
  int numInterpPoints = 0;
  int closed = 0;
  int numModes = 0;

  PyObject *pointsArg;
  if (!PyArg_ParseTuple(args,"Oiii",  &pointsArg,
      &closed,&numModes,&numInterpPoints))
  {
      PyErr_SetString(MathErr, "Could not import 1 tuple and 3 int: termsArg,closed,numModes,numInterpPoints");
      return NULL;
  }


  // Do work of command
  if (!PyList_Check(pointsArg)){
    PyErr_SetString( MathErr, "pointsArg is not a list");
    
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
     
  }

  // create result string
  char r[2048];
  PyObject *pylist=PyList_New(numInterpPoints);
  if(pylist!=NULL)
  {
  for (i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le %.6le",outPts[i][0],outPts[i][1],outPts[i][2]);
    PyObject *rr=PyBytes_FromString(r);
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
static PyMethodDef pyMath_methods[] = {
   {"FFT", pyMath_FFTCmd, METH_VARARGS,NULL},
   {"InverseFFT", pyMath_inverseFFTCmd, METH_VARARGS,NULL},
   {"ComputeWomersley", pyMath_computeWomersleyCmd, METH_VARARGS,NULL},
   {"LinearInterp", pyMath_linearInterpCmd, METH_VARARGS,NULL},
   {"CurveLength", pyMath_curveLengthCmd, METH_VARARGS,NULL},
   {"LinearInterpCurve", pyMath_linearInterpolateCurveCmd, METH_VARARGS,NULL},
   {"FitLeastSquares", pyMath_fitLeastSquaresCmd, METH_VARARGS,NULL},
   {"SmoothCurve", pyMath_smoothCurveCmd, METH_VARARGS,NULL},
  {NULL,       NULL},
  };

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyMathmodule = {
   PyModuleDef_HEAD_INIT,
   "pyMath",   /* name of module */
   "pyMath module", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyMath_methods
};
#endif
// --------------------
// initpyImage
// --------------------
#if PYTHON_MAJOR_VERSION == 2
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

#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyMath(void)
{
  PyObject *pyMth;

  pyMth = PyModule_Create(&pyMathmodule);
  printf("PyModule_Create called\n");
  if (pyMth==NULL) {
    printf("Error Creating Python module!\n");
    return NULL;
  }

MathErr = PyErr_NewException("pyMath.error",NULL,NULL);
Py_INCREF(MathErr);
PyModule_AddObject(pyMth,"error",MathErr);
Py_INCREF(pyMth);
return pyMth;

}
#endif
