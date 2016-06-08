/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
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


/** @file cv_occtsolid_init.cxx
 *  @brief Ipmlements function to register OCCTSolidModel as a solid type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "SimVascular.h"

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cv_solid_init.h"
#include "cv_occt_init.h"
#include "cvSolidModel.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cv_vtk_utils.h"
#include "cvOCCTSolidModel.h"

#include "cvFactoryRegistrar.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#ifdef SV_USE_PYTHON
#include "Python.h"
#include "vtkPythonUtil.h"
#include "PyVTKClass.h"
#endif
#include "cv_globals.h"
#include <TDF_Data.hxx>
#include <TDF_Label.hxx>
#include <TDocStd_Application.hxx>
#include <AppStd_Application.hxx>
#include <TDocStd_Document.hxx>
#include <TDocStd_XLinkTool.hxx>
#include <CDF_Session.hxx>
#include <XCAFDoc_DocumentTool.hxx>
#include <XCAFApp_Application.hxx>
#include "Standard_Version.hxx"

// Prototypes:
// -----------

cvOCCTSolidModel* CreateOCCTSolidModel()
{
	return new cvOCCTSolidModel();
}

// -----
// Solid
// -----
//
int OCCTSolidModel_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int OCCTSolidModel_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

#ifdef SV_USE_PYTHON
int OCCTSolidModel_InitPyModulesCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );
#endif


int Occtsolid_Init( Tcl_Interp *interp )
{
  Handle(XCAFApp_Application) OCCTManager = static_cast<XCAFApp_Application*>(gOCCTManager);
  //gOCCTManager = new AppStd_Application;
  OCCTManager = XCAFApp_Application::GetApplication();
  //if ( gOCCTManager == NULL ) {
  //  fprintf( stderr, "error allocating gOCCTManager\n" );
  //  return TCL_ERROR;
  //}
  Handle(TDocStd_Document) doc;
  //gOCCTManager->NewDocument("Standard",doc);
  OCCTManager->NewDocument("MDTV-XCAF",doc);
  if ( !XCAFDoc_DocumentTool::IsXCAFDocument(doc))
  {
    fprintf(stdout,"OCCT XDE is not setup correctly, file i/o and register of solid will not work correctly\n");
  }

  printf("  %-12s %s\n","OpenCASCADE:", OCC_VERSION_COMPLETE);
  cvFactoryRegistrar* solidModelRegistrar =
    (cvFactoryRegistrar *) Tcl_GetAssocData( interp, "SolidModelRegistrar", NULL);

  if (solidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          solidModelRegistrar->SetFactoryMethodPtr( SM_KT_OCCT,
      (FactoryMethodPtr) &CreateOCCTSolidModel );

    Tcl_CreateCommand( interp, "opencascade_available", OCCTSolidModel_AvailableCmd,
		       (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  }
  else {
    return TCL_ERROR;
  }

  Tcl_CreateCommand( interp, "opencascadesolidmodel_registrars", OCCTSolidModel_RegistrarsListCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
#ifdef SV_USE_PYTHON
  Tcl_CreateCommand( interp, "occt_initPyMods", OCCTSolidModel_InitPyModulesCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
#endif

  return TCL_OK;
}

int OCCTSolidModel_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  Tcl_SetResult( interp, "OpenCASCADE Solid Module Available", TCL_VOLATILE );

  return TCL_OK;
}

int OCCTSolidModel_RegistrarsListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  if ( argc != 1 ) {
    Tcl_SetResult( interp, "usage: registrars_list", TCL_STATIC );
    return TCL_ERROR;
  }
  cvFactoryRegistrar *solidModelRegistrar =
    (cvFactoryRegistrar *) Tcl_GetAssocData( interp, "SolidModelRegistrar", NULL);

  char result[255];
  sprintf( result, "Solid model registrar ptr -> %p\n", solidModelRegistrar );
  Tcl_AppendElement( interp, result );
  for (int i = 0; i < 5; i++) {
    sprintf( result, "GetFactoryMethodPtr(%i) = %p\n",
      i, (solidModelRegistrar->GetFactoryMethodPtr(i)));
    Tcl_AppendElement( interp, result );
  }
  return TCL_OK;
}

#ifdef SV_USE_PYTHON
// --------------------
// pySolid.convertListsToOCCTObject
// --------------------
PyObject* convertListsToOCCTObject(PyObject* self, PyObject* args)
{
  //Call cvOCCTSolidModel function to create BSpline surf
  cvOCCTSolidModel *geom;
  if (cvSolidModel::gCurrentKernel != SM_KT_OCCT)
  {
    fprintf(stderr,"Solid Model kernel must be OCCT\n");
    return NULL;
  }

  char *objName;
  int p=0,q=0;
  PyObject *X,*Y,*Z,*uKnots,*vKnots,*uMults,*vMults,*uDeg,*vDeg;
  if (!PyArg_ParseTuple(args,"sO!O!O!O!O!O!O!ii",&objName,
						&PyList_Type,&X,
					        &PyList_Type,&Y,
					        &PyList_Type,&Z,
					        &PyList_Type,&uKnots,
						&PyList_Type,&vKnots,
						&PyList_Type,&uMults,
						&PyList_Type,&vMults,
						&p,&q))
  {
    fprintf(stderr,"Could not import 1 char, 7 tuples, and 2 ints: X,Y,Z,uKnots,vKnots,uMults,vMults,uDeg,vDeg");
    return NULL;
  }

  geom = (cvOCCTSolidModel *)gRepository->GetObject( objName );
  if ( geom == NULL ) {
    fprintf(stderr,"Object is not in repository\n");
    return NULL;
  }
  //Get X,Y,Z arrays
  double **Xarr=NULL,**Yarr=NULL,**Zarr=NULL;
  int Xlen1=0,Xlen2=0,Ylen1=0,Ylen2=0,Zlen1=0,Zlen2=0;
  Py_INCREF(X); Py_INCREF(Y); Py_INCREF(Z);
  Xarr = getArrayFromDoubleList2D(X,Xlen1,Xlen2);
  Yarr = getArrayFromDoubleList2D(Y,Ylen1,Ylen2);
  Zarr = getArrayFromDoubleList2D(Z,Zlen1,Zlen2);
  Py_DECREF(X); Py_DECREF(Y); Py_DECREF(Z);
  //Clean up
  if ((Xlen1 != Ylen1 || Ylen1 != Zlen1 || Zlen1 != Xlen1) ||
      (Xlen2 != Ylen2 || Ylen2 != Zlen2 || Zlen2 != Xlen2))
  {
    fprintf(stderr,"X,Y,and Z inputs need to be same dimensions\n");
    for (int i=0;i<Xlen1;i++)
      delete [] Xarr[i];
    delete [] Xarr;
    for (int i=0;i<Ylen1;i++)
      delete [] Yarr[i];
    delete [] Yarr;
    for (int i=0;i<Zlen1;i++)
      delete [] Zarr[i];
    delete [] Zarr;
    return NULL;
  }

  //Get knots and multiplicity arrays
  double *uKarr=NULL,*vKarr=NULL,*uMarr=NULL,*vMarr=NULL;
  int uKlen=0,vKlen=0,uMlen=0,vMlen=0;
  uKarr = getArrayFromDoubleList(uKnots,uKlen);
  vKarr = getArrayFromDoubleList(vKnots,vKlen);
  uMarr = getArrayFromDoubleList(uMults,uMlen);
  vMarr = getArrayFromDoubleList(vMults,vMlen);

  if (geom->CreateBSplineSurface(Xarr,Yarr,Zarr,Xlen1,Xlen2,
    uKarr,uKlen,vKarr,vKlen,uMarr,uMlen,vMarr,vMlen,p,q) != CV_OK)
  {
    //Set special python thingy
    fprintf(stderr,"Conversion to BSpline surface didn't work\n");
    return NULL;
  }

  //Clean up
  for (int i=0;i<Xlen1;i++)
  {
    delete [] Xarr[i];
    delete [] Yarr[i];
    delete [] Zarr[i];
  }
  delete [] Xarr;
  delete [] Yarr;
  delete [] Zarr;

  delete [] uKarr;
  delete [] vKarr;
  delete [] uMarr;
  delete [] vMarr;

  return Py_BuildValue("s","success");
}
#endif


#ifdef SV_USE_PYTHON
//All functions listed and initiated as pyOCCT_methods declared here
// --------------------
// pyOCCT_methods
// --------------------
PyMethodDef pyOCCT_methods[] = {
  {"convertListsToOCCT", convertListsToOCCTObject, METH_VARARGS,"Converts X,Y,Z,uKnots,vKnots,uMults,vMults,p,q to OCCT"},
  {NULL, NULL}
};
#endif

#ifdef SV_USE_PYTHON
//Must be called after the python interpreter is initiated and through
//the tcl interprter. i.e. PyInterprter exec {tcl.eval("initPyMods")
// --------------------
// Solid_InitPyModules
// --------------------
int OCCTSolidModel_InitPyModulesCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *name;

  if ( argc != 1 ) {
    Tcl_SetResult( interp, "usage: occt_initPyMods", TCL_STATIC );
    return TCL_ERROR;
  }

  //Init our defined functions
  PyObject *pythonC;
  pythonC = Py_InitModule("pyOCCT", pyOCCT_methods);
  Py_INCREF(pythonC);
  PyModule_AddObject(PyImport_AddModule("__buildin__"), "pyOCCT", pythonC);

  return TCL_OK;
}
#endif

