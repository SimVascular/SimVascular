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


/** @file sv_occtsolid_init.cxx
 *  @brief Ipmlements function to register OCCTSolidModel as a solid type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "SimVascular.h"
#include "SimVascular_python.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_solid_init.h"
#include "sv_solid_init_py.h"
#include "sv_occt_init_py.h"
#include "sv_SolidModel.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"
#include "sv_OCCTSolidModel.h"

#include "sv_FactoryRegistrar.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif


// Globals:
// --------

#include "Python.h"
#include "vtkPythonUtil.h"
#if PYTHON_MAJOR_VERSION == 3
#include "PyVTKObject.h"
#elif PYTHON_MAJOR_VERSION == 2
#include "PyVTKClass.h"
#endif
#include "sv2_globals.h"
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

cvOCCTSolidModel* pyCreateOCCTSolidModel()
{
	return new cvOCCTSolidModel();
}

// -----
// Solid
// -----
//
PyObject* OCCTSolidModel_AvailableCmd( PyObject* self, PyObject* args);

PyObject* OCCTSolidModel_RegistrarsListCmd(PyObject* self, PyObject* args);

PyObject* convertListsToOCCTObject(PyObject* self, PyObject* args);

double *getArrayFromDoubleList(PyObject* listObj,int &len);

double **getArrayFromDoubleList2D(PyObject* listObj,int &lenx,int &leny);

PyMethodDef SolidOCCT_methods[] = {
  {"Available", OCCTSolidModel_AvailableCmd,METH_NOARGS,NULL},
  {"Registrars", OCCTSolidModel_RegistrarsListCmd,METH_NOARGS,NULL},
  {"convertListsToOCCT", convertListsToOCCTObject, METH_VARARGS,"Converts X,Y,Z,uKnots,vKnots,uMults,vMults,p,q to OCCT"},
  {NULL, NULL}
};

#if PYTHON_MAJOR_VERSION == 3

static struct PyModuleDef pySolidOCCTmodule = {
   PyModuleDef_HEAD_INIT,
   "pySolidOCCT",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   SolidOCCT_methods
};
#endif

PyObject* Occtsolid_pyInit()
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
  //get solidModelRegistrar from sys
  PyObject* pyGlobal = PySys_GetObject("solidModelRegistrar");
  pycvFactoryRegistrar* tmp = (pycvFactoryRegistrar *) pyGlobal;
  cvFactoryRegistrar* pySolidModelRegistrar =tmp->registrar;
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr(  SM_KT_OCCT,
            (FactoryMethodPtr) &pyCreateOCCTSolidModel );
  }
  else {
    return SV_PYTHON_ERROR;
  }
  tmp->registrar = pySolidModelRegistrar;
  PySys_SetObject("solidModelRegistrar",(PyObject*)tmp);
  PyObject *pythonC;
#if PYTHON_MAJOR_VERSION == 2
  pythonC = Py_InitModule("pySolidOCCT", SolidOCCT_methods);
#elif PYTHON_MAJOR_VERSION == 3
  pythonC = PyModule_Create(&pySolidOCCTmodule);
#endif  
if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolid");
    return SV_PYTHON_ERROR;
  }
  return pythonC;
}

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC
initpySolidOCCT()
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
  //get solidModelRegistrar from sys
  PyObject* pyGlobal = PySys_GetObject("solidModelRegistrar");
  pycvFactoryRegistrar* tmp = (pycvFactoryRegistrar *) pyGlobal;
  cvFactoryRegistrar* pySolidModelRegistrar =tmp->registrar;
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr(  SM_KT_OCCT,
            (FactoryMethodPtr) &pyCreateOCCTSolidModel );
  }
  else {
    return ;
  }
  tmp->registrar = pySolidModelRegistrar;
  PySys_SetObject("solidModelRegistrar",(PyObject*)tmp); 
  PyObject *pythonC;
  pythonC = Py_InitModule("pySolidOCCT", SolidOCCT_methods);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolid");
    return ;
  }
}
#endif

#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC
PyInit_pySolidOCCT()
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
  //get solidModelRegistrar from sys
  PyObject* pyGlobal = PySys_GetObject("solidModelRegistrar");
  pycvFactoryRegistrar* tmp = (pycvFactoryRegistrar *) pyGlobal;
  cvFactoryRegistrar* pySolidModelRegistrar =tmp->registrar;
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr(  SM_KT_OCCT,
            (FactoryMethodPtr) &pyCreateOCCTSolidModel );
  }
  else {
    return SV_PYTHON_ERROR;
  }
  tmp->registrar = pySolidModelRegistrar;
  PySys_SetObject("solidModelRegistrar",(PyObject*)tmp);
  PyObject *pythonC;

  pythonC = PyModule_Create(&pySolidOCCTmodule);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolid");
    pythonC;
  }

  return pythonC;
}
#endif

PyObject* OCCTSolidModel_AvailableCmd( PyObject* self, PyObject* args)
{
  return Py_BuildValue("s", "OpenCASCADE Solid Module Available");
}

PyObject* OCCTSolidModel_RegistrarsListCmd(PyObject* self, PyObject* args )
{
  char result[2048];
  int k=0;
  PyObject *pyPtr=PyList_New(6);
  PyObject* pyGlobal = PySys_GetObject("solidModelRegistrar");
  pycvFactoryRegistrar* tmp = (pycvFactoryRegistrar *) pyGlobal;
  cvFactoryRegistrar* pySolidModelRegistrar =tmp->registrar;
  sprintf(result, "Solid model registrar ptr -> %p\n", pySolidModelRegistrar);
  fprintf(stdout,result);
  PyList_SetItem(pyPtr,0,PyString_FromFormat(result));

  for (int i = 0; i < 5; i++) {
      sprintf( result,"GetFactoryMethodPtr(%i) = %p\n",
      i, (pySolidModelRegistrar->GetFactoryMethodPtr(i)));
      fprintf(stdout,result);
      PyList_SetItem(pyPtr,i+1,PyString_FromFormat(result));
  }
  return pyPtr;
}


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
    uKarr,uKlen,vKarr,vKlen,uMarr,uMlen,vMarr,vMlen,p,q) != SV_OK)
  {
    //Set special python thingy
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

  return SV_PYTHON_OK;
}

// --------------------
// getArrayFromDoubleList
// --------------------
double *getArrayFromDoubleList(PyObject* listObj,int &len)
{
  len = PyList_Size(listObj);
  double *arr = new double[len];
  for (int i=0;i<len;i++)
  {
    arr[i] = (double) PyFloat_AsDouble(PyList_GetItem(listObj,i));
  }
  return arr;
}


// --------------------
// getArrayFromDoubleList2D
// --------------------
double **getArrayFromDoubleList2D(PyObject* listObj,int &lenx,int &leny)
{
  lenx = PyList_Size(listObj);
  double **arr = new double*[lenx];
  for (int i=0;i<lenx;i++)
  {
    PyObject *newList = PyList_GetItem(listObj,i);
    leny = PyList_Size(newList);
    arr[i] = new double[leny];
    for (int j=0;j<leny;j++)
      arr[i][j] = (double) PyFloat_AsDouble(PyList_GetItem(newList,j));
  }
  return arr;
}



