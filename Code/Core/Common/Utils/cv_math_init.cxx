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

int Math_FFTCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] );

int Math_inverseFFTCmd( ClientData clientData, Tcl_Interp *interp,
		int argc, CONST84 char *argv[] );

int Math_computeWomersleyCmd( ClientData clientData, Tcl_Interp *interp,
		int argc, CONST84 char *argv[] );

int Math_linearInterpCmd( ClientData clientData, Tcl_Interp *interp,
		int argc, CONST84 char *argv[] );

int Math_curveLengthCmd( ClientData clientData, Tcl_Interp *interp,
		int argc, CONST84 char *argv[] );

int Math_linearInterpolateCurveCmd( ClientData clientData, Tcl_Interp *interp,
		int argc, CONST84 char *argv[] );

int Math_fitLeastSquaresCmd( ClientData clientData, Tcl_Interp *interp,
                int argc, CONST84 char *argv[] );

int Math_smoothCurveCmd( ClientData clientData, Tcl_Interp *interp,
                int argc, CONST84 char *argv[] );

// ---------
// Math_Init
// ---------

int Math_Init( Tcl_Interp *interp )
{
  Tcl_CreateCommand( interp, "math_FFT", Math_FFTCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "math_inverseFFT", Math_inverseFFTCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "math_computeWomersley", Math_computeWomersleyCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "math_linearInterp", Math_linearInterpCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "math_curveLength", Math_curveLengthCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "math_linearInterpCurve", Math_linearInterpolateCurveCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "math_fitLeastSquares", Math_fitLeastSquaresCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "math_smoothCurve", Math_smoothCurveCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  return TCL_OK;
}


// -----------
// Math_FFTCmd
// -----------

int Math_FFTCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;

  int nterms = 0;
  int numInterpPoints = 0;
  ARG_List pointsArg;

  int table_sz = 3;
  ARG_Entry arg_table[] = {
    { "-pts", LIST_Type, &pointsArg, NULL, REQUIRED, 0, { 0 } },
    { "-nterms", INT_Type, &nterms, NULL, REQUIRED, 0, { 0 } },
    { "-numInterpPts", INT_Type, &numInterpPoints, NULL, REQUIRED, 0, { 0 } },
  };

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

  int nlistpts = pointsArg.argc;
  int npts = 0;

  ARG_List *indpts = new ARG_List [nlistpts];

  if ( ARG_ParseTclListStatic( interp, pointsArg, LIST_Type, indpts, nlistpts, &npts )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    delete indpts;
    return TCL_ERROR;
  }

  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,2);
  double pt[2];

  for (i = 0; i < nlistpts; i++) {
    if ( ARG_ParseTclListStatic( interp, indpts[i], DOUBLE_Type, pt, 2, &npts )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,2);
      delete mathobj;
      return TCL_ERROR;
    }
    if ( npts != 2 ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,2);
      delete mathobj;
      return TCL_ERROR;
    }
    pts[i][0] = pt[0];
    pts[i][1] = pt[1];    
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  double **terms = NULL;
  if ((mathobj->FFT(pts, nlistpts, numInterpPoints, nterms, &terms)) == CV_ERROR) {
     Tcl_SetResult( interp, "error in fft", TCL_VOLATILE );
     ARG_FreeListArgvs( table_sz, arg_table );
     delete indpts;
     mathobj->deleteArray(pts,nlistpts,2);
     delete mathobj;
     return TCL_ERROR;
  }
  
  // create result string
  char r[2048];
  for (i = 0; i < nterms; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le",terms[i][0],terms[i][1]);
    Tcl_AppendElement(interp, r);
  }

  // clean up
  ARG_FreeListArgvs( table_sz, arg_table );
  delete indpts;
  mathobj->deleteArray(pts,nlistpts,2);
  mathobj->deleteArray(terms,nterms,2);
  delete mathobj;

  return TCL_OK;
}


// ------------------
// Math_inverseFFTCmd
// ------------------

int Math_inverseFFTCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;
  ARG_List termsArg;
  double t0 = 0;
  double dt = 0;
  double omega = 0;
  int numPts = 0;
 
  int table_sz = 5;
  ARG_Entry arg_table[] = {
    { "-terms", LIST_Type, &termsArg, NULL, REQUIRED, 0, { 0 } },
    { "-t0", DOUBLE_Type, &t0, NULL, REQUIRED, 0, { 0 } },
    { "-dt", DOUBLE_Type, &dt, NULL, REQUIRED, 0, { 0 } },
    { "-omega", DOUBLE_Type, &omega, NULL, REQUIRED, 0, { 0 } },
    { "-numPts", INT_Type, &numPts, NULL, REQUIRED, 0, { 0 } },
  };

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

  int nlistterms = termsArg.argc;
  int numTerms = 0;

  ARG_List *indterms = new ARG_List [nlistterms];

  if ( ARG_ParseTclListStatic( interp, termsArg, LIST_Type, indterms, nlistterms, &numTerms )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    delete indterms;
    return TCL_ERROR;
  }

  int i;
  cvMath *mathobj = new cvMath();
  double **terms = mathobj->createArray(nlistterms,2);
  double term[2];
  int nt = 0;

  for (i = 0; i < nlistterms; i++) {
    if ( ARG_ParseTclListStatic( interp, indterms[i], DOUBLE_Type, term, 2, &nt )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in terms list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indterms;
      mathobj->deleteArray(terms,nlistterms,2);
      delete mathobj;
      return TCL_ERROR;
    }
    if ( nt != 2 ) {
      Tcl_SetResult( interp, "error in terms list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indterms;
      mathobj->deleteArray(terms,nlistterms,2);
      delete mathobj;
      return TCL_ERROR;
    }
    terms[i][0] = term[0];
    terms[i][1] = term[1];    
  }

  //for (i = 0; i < nlistterms; i++) {
  //     fprintf(stdout,"Term %i:  %lf %lf\n",i,terms[i][0],terms[i][1]);
  //}

  double **pts = NULL;
  if ( (mathobj->inverseFFT(terms, nlistterms, t0, dt, omega, numPts, &pts)) == CV_ERROR) {
     Tcl_SetResult( interp, "error in inverse fft", TCL_VOLATILE );
     ARG_FreeListArgvs( table_sz, arg_table );
     delete indterms;
     mathobj->deleteArray(terms,nlistterms,2);
     delete mathobj;
     return TCL_ERROR;
  }
  
  // create result string
  char r[2048];
  for (i = 0; i < numPts; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le",pts[i][0],pts[i][1]);
    Tcl_AppendElement(interp, r);
  }

  // clean up
  ARG_FreeListArgvs( table_sz, arg_table );
  delete indterms;
  mathobj->deleteArray(terms,nlistterms,2);
  mathobj->deleteArray(pts,numPts,2);
  delete mathobj;

  return TCL_OK;
}


// ------------------------
// Math_computeWomersleyCmd
// ------------------------

int Math_computeWomersleyCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;
  ARG_List termsArg;
  double time = 0;
  double viscosity = 0;
  double omega = 0;
  double density = 0;
  double radmax = 0;
  double radius = 0;
 
  int table_sz = 7;
  ARG_Entry arg_table[] = {
    { "-terms", LIST_Type, &termsArg, NULL, REQUIRED, 0, { 0 } },
    { "-time", DOUBLE_Type, &time, NULL, REQUIRED, 0, { 0 } },
    { "-viscosity", DOUBLE_Type, &viscosity, NULL, REQUIRED, 0, { 0 } },
    { "-omega", DOUBLE_Type, &omega, NULL, REQUIRED, 0, { 0 } },
    { "-density", DOUBLE_Type, &density, NULL, REQUIRED, 0, { 0 } },
    { "-radmax", DOUBLE_Type, &radmax, NULL, REQUIRED, 0, { 0 } },
    { "-radius", DOUBLE_Type, &radius, NULL, REQUIRED, 0, { 0 } },
  };

 
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

  int nlistterms = termsArg.argc;
  int numTerms = 0;

  ARG_List *indterms = new ARG_List [nlistterms];

  if ( ARG_ParseTclListStatic( interp, termsArg, LIST_Type, indterms, nlistterms, &numTerms )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    delete indterms;
    return TCL_ERROR;
  }

  int i;
  cvMath *mathobj = new cvMath();
  double **terms = mathobj->createArray(nlistterms,2);
  double term[2];
  int nt = 0;

  for (i = 0; i < nlistterms; i++) {
    if ( ARG_ParseTclListStatic( interp, indterms[i], DOUBLE_Type, term, 2, &nt )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in terms list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indterms;
      mathobj->deleteArray(terms,nlistterms,2);
      delete mathobj;
      return TCL_ERROR;
    }
    if ( nt != 2 ) {
      Tcl_SetResult( interp, "error in terms list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indterms;
      mathobj->deleteArray(terms,nlistterms,2);
      delete mathobj;
      return TCL_ERROR;
    }
    terms[i][0] = term[0];
    terms[i][1] = term[1];    
  }

  //for (i = 0; i < nlistterms; i++) {
  //     fprintf(stdout,"Term %i:  %lf %lf\n",i,terms[i][0],terms[i][1]);
  //}

  double velocity = 0;
  if ((mathobj->compute_v_womersley(terms, nlistterms, viscosity, density,
                     omega, radmax, radius, time, &velocity)) == CV_ERROR) {
     Tcl_SetResult( interp, "error in calculate worm", TCL_VOLATILE );
     ARG_FreeListArgvs( table_sz, arg_table );
     delete indterms;
     mathobj->deleteArray(terms,nlistterms,2);
     delete mathobj;
     return TCL_ERROR;
  }
  
  // create result string
  char r[2048];
  r[0] = '\0';
  sprintf(r,"%.6le",velocity);
  Tcl_AppendElement(interp, r);

  // clean up
  ARG_FreeListArgvs( table_sz, arg_table );
  delete indterms;
  mathobj->deleteArray(terms,nlistterms,2);
  delete mathobj;

  return TCL_OK;
}


int Math_linearInterpCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;
  int numInterpPoints = 0;
  ARG_List pointsArg;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-pts", LIST_Type, &pointsArg, NULL, REQUIRED, 0, { 0 } },
    { "-numInterpPts", INT_Type, &numInterpPoints, NULL, REQUIRED, 0, { 0 } },
  };

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

  int nlistpts = pointsArg.argc;
  int npts = 0;

  ARG_List *indpts = new ARG_List [nlistpts];

  if ( ARG_ParseTclListStatic( interp, pointsArg, LIST_Type, indpts, nlistpts, &npts )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    delete indpts;
    return TCL_ERROR;
  }

  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,2);
  double pt[2];

  for (i = 0; i < nlistpts; i++) {
    if ( ARG_ParseTclListStatic( interp, indpts[i], DOUBLE_Type, pt, 2, &npts )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,2);
      delete mathobj;
      return TCL_ERROR;
    }
    if ( npts != 2 ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,2);
      delete mathobj;
      return TCL_ERROR;
    }
    pts[i][0] = pt[0];
    pts[i][1] = pt[1];    
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  // here we calculate dt so that our time series will go from
  // 0 to T.
  double t0 = pts[0][0];
  double dt = (pts[nlistpts-1][0]-t0)/(numInterpPoints-1);
  double **outPts = NULL;

  if ((mathobj->linearInterpolate(pts, nlistpts, t0, dt, numInterpPoints, &outPts)) == CV_ERROR) {
     Tcl_SetResult( interp, "error in linear interpolation", TCL_VOLATILE );
     ARG_FreeListArgvs( table_sz, arg_table );
     delete indpts;
     mathobj->deleteArray(pts,nlistpts,2);
     delete mathobj;
     return TCL_ERROR;
  }
  
  // create result string
  char r[2048];
  for (i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le",outPts[i][0],outPts[i][1]);
    Tcl_AppendElement(interp, r);
  }

  // clean up
  ARG_FreeListArgvs( table_sz, arg_table );
  delete indpts;
  mathobj->deleteArray(pts,nlistpts,2);
  mathobj->deleteArray(outPts,numInterpPoints,2);
  delete mathobj;

  return TCL_OK;
}


int Math_curveLengthCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;
  ARG_List pointsArg;
  int closed = 0;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-pts", LIST_Type, &pointsArg, NULL, REQUIRED, 0, { 0 } },
    { "-closed", INT_Type, &closed, NULL, REQUIRED, 0, { 0 } },
  };

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

  int nlistpts = pointsArg.argc;
  int npts = 0;

  ARG_List *indpts = new ARG_List [nlistpts];

  if ( ARG_ParseTclListStatic( interp, pointsArg, LIST_Type, indpts, nlistpts, &npts )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    delete indpts;
    return TCL_ERROR;
  }

  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,3);
  double pt[3];

  for (i = 0; i < nlistpts; i++) {
    if ( ARG_ParseTclListStatic( interp, indpts[i], DOUBLE_Type, pt, 3, &npts )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,3);
      delete mathobj;
      return TCL_ERROR;
    }
    if ( npts != 3 ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,3);
      delete mathobj;
      return TCL_ERROR;
    }
    pts[i][0] = pt[0];
    pts[i][1] = pt[1]; 
    pts[i][2] = pt[2];   
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  double length = 0;
  if ((mathobj->curveLength(pts, nlistpts, closed, &length)) == CV_ERROR) {
     Tcl_SetResult( interp, "error finding curve length", TCL_VOLATILE );
     ARG_FreeListArgvs( table_sz, arg_table );
     delete indpts;
     mathobj->deleteArray(pts,nlistpts,3);
     delete mathobj;
     return TCL_ERROR;
  }
  
  // create result string
  char r[2048];
  r[0] = '\0';
  sprintf(r,"%.6le",length);
  Tcl_AppendElement(interp, r);

  // clean up
  ARG_FreeListArgvs( table_sz, arg_table );
  delete indpts;
  mathobj->deleteArray(pts,nlistpts,3);
  delete mathobj;

  return TCL_OK;
}


int Math_linearInterpolateCurveCmd( ClientData clientData, Tcl_Interp *interp,
			            int argc, CONST84 char *argv[] )
{
  char *usage;
  int numInterpPoints = 0;
  int closed = 0;
  ARG_List pointsArg;

  int table_sz = 3;
  ARG_Entry arg_table[] = {
    { "-pts", LIST_Type, &pointsArg, NULL, REQUIRED, 0, { 0 } },
    { "-closed", INT_Type, &closed, NULL, REQUIRED, 0, { 0 } },
    { "-numInterpPts", INT_Type, &numInterpPoints, NULL, REQUIRED, 0, { 0 } },
  };

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

  int nlistpts = pointsArg.argc;
  int npts = 0;

  ARG_List *indpts = new ARG_List [nlistpts];

  if ( ARG_ParseTclListStatic( interp, pointsArg, LIST_Type, indpts, nlistpts, &npts )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    delete indpts;
    return TCL_ERROR;
  }

  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,3);
  double pt[3];

  for (i = 0; i < nlistpts; i++) {
    if ( ARG_ParseTclListStatic( interp, indpts[i], DOUBLE_Type, pt, 3, &npts )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,3);
      delete mathobj;
      return TCL_ERROR;
    }
    if ( npts != 3 ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,3);
      delete mathobj;
      return TCL_ERROR;
    }
    pts[i][0] = pt[0];
    pts[i][1] = pt[1];
    pts[i][2] = pt[2];    
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  double **outPts = NULL;
  if ((mathobj->linearInterpolateCurve(pts, nlistpts, closed, numInterpPoints, &outPts)) == CV_ERROR) {
     Tcl_SetResult( interp, "error in linear interpolation", TCL_VOLATILE );
     ARG_FreeListArgvs( table_sz, arg_table );
     delete indpts;
     mathobj->deleteArray(pts,nlistpts,3);
     delete mathobj;
     return TCL_ERROR;
  }
  
  // create result string
  char r[2048];
  for (i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le %.6le",outPts[i][0],outPts[i][1],outPts[i][2]);
    Tcl_AppendElement(interp, r);
  }

  // clean up
  ARG_FreeListArgvs( table_sz, arg_table );
  delete indpts;
  mathobj->deleteArray(pts,nlistpts,3);
  mathobj->deleteArray(outPts,numInterpPoints,3);
  delete mathobj;

  return TCL_OK;
}


// -----------------------
// Math_fitLeastSquaresCmd
// -----------------------

int Math_fitLeastSquaresCmd( ClientData clientData, Tcl_Interp *interp,
			int argc, CONST84 char *argv[] )
{
  char *usage;
  ARG_List xtermsArg,ytermsArg;
  int xOrder = 0;
  int yOrder = 0;
 
  int table_sz = 4;
  ARG_Entry arg_table[] = {
    { "-X", LIST_Type, &xtermsArg, NULL, REQUIRED, 0, { 0 } },
    { "-Y", LIST_Type, &ytermsArg, NULL, REQUIRED, 0, { 0 } },
    { "-xOrder", INT_Type, &xOrder, NULL, REQUIRED, 0, { 0 } },
    { "-yOrder", INT_Type, &yOrder, NULL, REQUIRED, 0, { 0 } },
  };

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
  if (xtermsArg.argc != ytermsArg.argc) {
      fprintf(stderr,"ERROR:  X and Y must have the same number of samples (%i != %i).\n",xtermsArg.argc,ytermsArg.argc);
      Tcl_SetResult( interp, usage, TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
  }

  int numberOfSamples = xtermsArg.argc;
  ARG_List *xterms = new ARG_List [numberOfSamples];
  int numTerms = 0;

  if ( ARG_ParseTclListStatic( interp, xtermsArg, LIST_Type, xterms, numberOfSamples, &numTerms )
       != TCL_OK ) {
    Tcl_SetResult( interp, "error in X samples", TCL_VOLATILE );
    delete xterms;
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }

  int i,j;
  cvMath *mathobj = new cvMath();

  double **xt = mathobj->createArray(numberOfSamples,xOrder);
  for (i = 0; i < numberOfSamples; i++) {
    if ( ARG_ParseTclListStatic( interp, xterms[i], DOUBLE_Type, xt[i], xOrder, &numTerms )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in X samples", TCL_VOLATILE );
      mathobj->deleteArray(xt,numberOfSamples,xOrder);
      delete mathobj;
      delete xterms;
      ARG_FreeListArgvs( table_sz, arg_table );
      return TCL_ERROR;
    }
    if (numTerms != xOrder) {
      Tcl_SetResult( interp, "error in X samples", TCL_VOLATILE );
      mathobj->deleteArray(xt,numberOfSamples,xOrder);
      delete mathobj;
      delete xterms;
      ARG_FreeListArgvs( table_sz, arg_table );
      return TCL_ERROR;
    }  
  }     

  delete xterms;

  ARG_List *yterms = new ARG_List [numberOfSamples];

  if ( ARG_ParseTclListStatic( interp, ytermsArg, LIST_Type, yterms, numberOfSamples, &numTerms )
       != TCL_OK ) {
    Tcl_SetResult( interp, "error in Y samples", TCL_VOLATILE );
    delete yterms;
    ARG_FreeListArgvs( table_sz, arg_table );
    return TCL_ERROR;
  }

  double **yt = mathobj->createArray(numberOfSamples,yOrder);
  for (i = 0; i < numberOfSamples; i++) {
    if ( ARG_ParseTclListStatic( interp, yterms[i], DOUBLE_Type, yt[i], yOrder, &numTerms )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in Y samples", TCL_VOLATILE );
      mathobj->deleteArray(xt,numberOfSamples,xOrder);
      mathobj->deleteArray(yt,numberOfSamples,yOrder);
      delete mathobj;
      delete yterms;
      ARG_FreeListArgvs( table_sz, arg_table );
      return TCL_ERROR;
    }
    if (numTerms != yOrder) {
      Tcl_SetResult( interp, "error in Y samples", TCL_VOLATILE );
      mathobj->deleteArray(xt,numberOfSamples,xOrder);
      mathobj->deleteArray(yt,numberOfSamples,yOrder);
      delete mathobj;
      delete yterms;
      ARG_FreeListArgvs( table_sz, arg_table );
      return TCL_ERROR;
    }
  }

  delete yterms;
  ARG_FreeListArgvs( table_sz, arg_table );

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
  if ( (mathobj->fitLeastSquares(numberOfSamples,xt,xOrder,yt,yOrder,mt)) == CV_ERROR) {
     Tcl_SetResult( interp, "error in least squares fit", TCL_VOLATILE );
      mathobj->deleteArray(xt,numberOfSamples,xOrder);
      mathobj->deleteArray(yt,numberOfSamples,yOrder);
      delete mathobj;
     return TCL_ERROR;
  }

  // create result string
  char r[2048];
  for (i = 0; i < xOrder; i++) {
      r[0] = '\0';
      for (j = 0; j < yOrder; j++) {
        sprintf(r,"%.6le ",mt[i][j]);
      }
      Tcl_AppendElement(interp, r);
  }

  // clean up
  mathobj->deleteArray(xt,numberOfSamples,xOrder);
  mathobj->deleteArray(yt,numberOfSamples,yOrder);
  mathobj->deleteArray(mt,xOrder,yOrder);
  delete mathobj;

  return TCL_OK;
}



int Math_smoothCurveCmd( ClientData clientData, Tcl_Interp *interp,
			            int argc, CONST84 char *argv[] )
{
  char *usage;
  int numInterpPoints = 0;
  int closed = 0;
  int numModes = 0;
  ARG_List pointsArg;

  int table_sz = 4;
  ARG_Entry arg_table[] = {
    { "-pts", LIST_Type, &pointsArg, NULL, REQUIRED, 0, { 0 } },
    { "-closed", INT_Type, &closed, NULL, REQUIRED, 0, { 0 } },
    { "-numModes", INT_Type, &numModes, NULL, REQUIRED, 0, { 0 } },
    { "-numInterpPts", INT_Type, &numInterpPoints, NULL, REQUIRED, 0, { 0 } },
  };

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

  int nlistpts = pointsArg.argc;
  int npts = 0;

  ARG_List *indpts = new ARG_List [nlistpts];

  if ( ARG_ParseTclListStatic( interp, pointsArg, LIST_Type, indpts, nlistpts, &npts )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    ARG_FreeListArgvs( table_sz, arg_table );
    delete indpts;
    return TCL_ERROR;
  }

  int i;
  cvMath *mathobj = new cvMath();
  double **pts = mathobj->createArray(nlistpts,3);
  double pt[3];

  for (i = 0; i < nlistpts; i++) {
    if ( ARG_ParseTclListStatic( interp, indpts[i], DOUBLE_Type, pt, 3, &npts )
       != TCL_OK ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,3);
      delete mathobj;
      return TCL_ERROR;
    }
    if ( npts != 3 ) {
      Tcl_SetResult( interp, "error in points list", TCL_VOLATILE );
      ARG_FreeListArgvs( table_sz, arg_table );
      delete indpts;
      mathobj->deleteArray(pts,nlistpts,3);
      delete mathobj;
      return TCL_ERROR;
    }
    pts[i][0] = pt[0];
    pts[i][1] = pt[1];
    pts[i][2] = pt[2];    
  }

  //for (i = 0; i < nlistpts; i++) {
  //     fprintf(stdout,"Point %i:  %lf %lf\n",i,pts[i][0],pts[i][1]);
  //}

  double **outPts = NULL;
  if ((mathobj->smoothCurve(pts, nlistpts, closed, numModes, numInterpPoints, &outPts)) == CV_ERROR) {
     Tcl_SetResult( interp, "error in smoothing curve", TCL_VOLATILE );
     ARG_FreeListArgvs( table_sz, arg_table );
     delete indpts;
     mathobj->deleteArray(pts,nlistpts,3);
     delete mathobj;
     return TCL_ERROR;
  }
  
  // create result string
  char r[2048];
  for (i = 0; i < numInterpPoints; i++) {
    r[0] = '\0';
    sprintf(r,"%.6le %.6le %.6le",outPts[i][0],outPts[i][1],outPts[i][2]);
    Tcl_AppendElement(interp, r);
  }

  // clean up
  ARG_FreeListArgvs( table_sz, arg_table );
  delete indpts;
  mathobj->deleteArray(pts,nlistpts,3);
  mathobj->deleteArray(outPts,numInterpPoints,3);
  delete mathobj;

  return TCL_OK;
}

