/* Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
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
#include "Python.h"

#include "cv_repos_init_py.h"
#include "cvRepository.h"
#include "cvPolyData.h"
#include "cvStrPts.h"
#include "cvUnstructuredGrid.h"
#include "cv_arg.h"
#include "cvVTK.h"
#include "vtkTclUtil.h"
#include "vtkPythonUtil.h"
#ifdef SV_USE_PYTHON
#include "Python.h"
#endif

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// The global cvRepository object should be allocated in the file where
// main() lives.  ... Why?

#include "cv_globals.h"
//#include <unistd.h>

// Prototypes:

PyObject* PyRunTimeErr;

PyObject* Repos_ListCmd( PyObject* self, PyObject* args);

PyObject* Repos_ExistsCmd( PyObject* self, PyObject* args);

PyObject* Repos_DeleteCmd( PyObject* self, PyObject* args);

PyObject* Repos_TypeCmd( PyObject* self, PyObject* args );

PyObject* Repos_ImportVtkPdCmd( PyObject* self, PyObject* args);

PyObject* Repos_ImportVtkSpCmd( PyObject* self, PyObject* args );

PyObject* Repos_ImportVtkImgCmd( PyObject* self, PyObject* args);

PyObject* Repos_ImportVtkUnstructuredGridCmd( PyObject* self, PyObject* args );

PyObject*  Repos_ExportToVtkCmd( PyObject* self, PyObject* args );

PyObject*  Repos_SaveCmd( PyObject* self, PyObject* args );

PyObject*  Repos_LoadCmd( PyObject* self, PyObject* args );

PyObject* Repos_WriteVtkPolyDataCmd( PyObject* self, PyObject* args);

PyObject* Repos_ReadVtkPolyDataCmd( PyObject* self, PyObject* args);

PyObject*  Repos_WriteVtkStructuredPointsCmd( PyObject* self, PyObject* args);

PyObject*  Repos_WriteVtkUnstructuredGridCmd( PyObject* self, PyObject* args);

PyObject*  Repos_SetStringCmd( PyObject* self, PyObject* args);

PyObject* Repos_GetStringCmd( PyObject* self, PyObject* args);

PyMODINIT_FUNC initpyRepository(void);

// Label-related methods
// ---------------------

static PyObject* Repos_GetLabelKeysCmd( PyObject* self, PyObject* args );

static PyObject* Repos_GetLabelCmd( PyObject* self, PyObject* args);

static PyObject* Repos_SetLabelCmd( PyObject* self, PyObject* args);

static PyObject* Repos_ClearLabelCmd( PyObject* self, PyObject* args );
// ----------
// Repos_Methods
// ----------
PyMethodDef pyRepository_methods[] =

{

    {"repos_list", Repos_ListCmd, METH_NOARGS,NULL},
    {"repos_exists", Repos_ExistsCmd, METH_VARARGS,NULL},
    {"repos_delete", Repos_DeleteCmd, METH_VARARGS,NULL},
    {"repos_type", Repos_TypeCmd, METH_VARARGS,NULL},
    {"repos_importVtkPd", Repos_ImportVtkPdCmd, METH_VARARGS,NULL},
    {"repos_exportToVtk", Repos_ExportToVtkCmd, METH_VARARGS,NULL},
    {"repos_importVtkSp", Repos_ImportVtkSpCmd, METH_VARARGS,NULL},
    {"repos_importVtkImg", Repos_ImportVtkImgCmd, METH_VARARGS,NULL},
    {"repos_importVtkUnstructuredGrid", Repos_ImportVtkUnstructuredGridCmd,
      METH_VARARGS,NULL},
    {"repos_save", Repos_SaveCmd, METH_VARARGS,NULL},
    {"repos_load", Repos_LoadCmd, METH_VARARGS,NULL},
    {"repos_writeVtkPolyData", Repos_WriteVtkPolyDataCmd, METH_VARARGS,NULL},
    {"repos_readVtkPolyData", Repos_ReadVtkPolyDataCmd, METH_VARARGS,NULL},
    {"repos_writeVtkStructuredPoints", Repos_WriteVtkStructuredPointsCmd,
     METH_VARARGS,NULL},
    {"repos_getLabelKeys", Repos_GetLabelKeysCmd, METH_VARARGS,NULL},
    {"repos_getLabel", Repos_GetLabelCmd, METH_VARARGS,NULL},
    {"repos_setLabel", Repos_SetLabelCmd, METH_VARARGS,NULL},
    {"repos_clearLabel", Repos_ClearLabelCmd, METH_VARARGS,NULL},
    {"repos_setstring", Repos_SetStringCmd, METH_VARARGS,NULL},
    {"repos_getstring", Repos_GetStringCmd, METH_VARARGS,NULL},
    {NULL, NULL,0,NULL},

};

PyMODINIT_FUNC initpyRepository(void)

{

  PyObject *pyRepo;
  gRepository = new cvRepository();
  if ( gRepository == NULL ) {
    fprintf( stderr, "error allocating gRepository\n" );
    return;
  }
  pyRepo = Py_InitModule("pyRepository",pyRepository_methods);

  PyRunTimeErr = PyErr_NewException("pyRepository.error",NULL,NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(pyRepo,"error",PyRunTimeErr);

}


int Repos_pyInit()

{

  Py_Initialize();
  initpyRepository();
  return SV_OK;

}

// -------------
// Repos_PassCmd
// -------------

PyObject* Repos_SetStringCmd( PyObject* self, PyObject* args)

{

  char *stringPd;
  char *string;
  cvRepositoryData *pd;

  if (!PyArg_ParseTuple(args,"ss", &stringPd, &string))
  {
  PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: stringPd, string");
  return SV_ERROR;
  }

  // Retrieve source object:
  pd = gRepository->GetObject( stringPd );
  if ( pd == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object stringPd");
    return SV_ERROR;
  }

  pd->SetName( string );

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%s", string );
  return Py_BuildValue("s",rtnstr);

}

PyObject* Repos_GetStringCmd( PyObject* self, PyObject* args)

{

  char *stringPd;
  cvRepositoryData *pd;

  if (!PyArg_ParseTuple(args,"s", &stringPd))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: stringPd");
    return SV_ERROR;
  }

  // Retrieve source object:
  pd = gRepository->GetObject( stringPd );
  if ( pd == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object stringPd");
    return SV_ERROR;
  }
  return Py_BuildValue("s",pd->GetName());

}

// -------------
// Repos_ListCmd
// -------------

PyObject* Repos_ListCmd( PyObject* self, PyObject* args)

{

  char *name;

  PyObject *pylist=PyList_New(0);
  gRepository->InitIterator();
  while ( name = gRepository->GetNextName() )
  {
    PyObject* pyName = PyString_FromString(name);
    PyList_Append( pylist, pyName );
  }
  return pylist;

}


// ---------------
// Repos_ExistsCmd
// ---------------

PyObject*  Repos_ExistsCmd( PyObject* self, PyObject* args)

{

  char *name;
  int exists;

  if (!PyArg_ParseTuple(args,"s", &name))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: name");
    return SV_ERROR;
  }

  exists = gRepository->Exists( name );

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", exists );

  return Py_BuildValue("s",rtnstr);

}

// ---------------
// Repos_DeleteCmd
// ---------------

PyObject*  Repos_DeleteCmd( PyObject* self, PyObject* args)

{

  char *objName;
  int exists;
  int unreg_status;
  RepositoryDataT obj_t;

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: objName");
    return SV_ERROR;
  }

  // Do work of command:
  char r[2048];
  exists = gRepository->Exists( objName );
  if ( ! exists ) {
    r[0] = '\0';
    sprintf(r,"object %s not in repository",objName);
    PyErr_SetString(PyRunTimeErr, r);
    return SV_ERROR;
  }

  obj_t = gRepository->GetType( objName );
  if ( obj_t == SOLID_MODEL_T || obj_t == MESH_T || obj_t == ADAPTOR_T) {
    unreg_status = gRepository->UnRegister( objName );
  }

  if ( ! unreg_status )
  {
    r[0] = '\0';
    sprintf(r,"error deleting object %s", objName);
    PyErr_SetString(PyRunTimeErr, r);
    return SV_ERROR;
  }
  return Py_BuildValue("s","success");

}


// -------------
// Repos_TypeCmd
// -------------

PyObject* Repos_TypeCmd( PyObject* self, PyObject* args)

{

  char *name;
  RepositoryDataT type;
  char *typeStr;

  if (!PyArg_ParseTuple(args,"s", &name))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: name");
    return SV_ERROR;
  }

  char r[2048];
  if ( ! gRepository->Exists( name ) )
  {
    r[0] = '\0';
    sprintf(r, "object %s not found", name);
    PyErr_SetString(PyRunTimeErr,r);
    return SV_ERROR;
  }

  type = gRepository->GetType( name );
  typeStr = RepositoryDataT_EnumToStr( type );
  //delete [] typeStr;
  return Py_BuildValue("s",typeStr);

}


// --------------------
// Repos_ImportVtkPdCmd
// --------------------
// The idea here is that we want to take an interpreter-level name for
// a vtk object, and use that to look up a pointer to that object and
// then to create a cvRepositoryData object from that pointer.

PyObject* Repos_ImportVtkPdCmd( PyObject* self, PyObject* args)

{

  PyObject *vtkName;
  char *objName;
  vtkPolyData *vtkObj;
  int status;
  cvPolyData *pd;

  if (!PyArg_ParseTuple(args,"Os", &vtkName,&objName))
  {
    PyErr_SetString(PyRunTimeErr,
      "Could not import 1 tuple and 1 char: vtkName,name");
    return SV_ERROR;
  }

  // Look up the named vtk object:
  vtkObj = (vtkPolyData *)vtkPythonUtil::GetPointerFromObject( vtkName,
    "vtkPolyData");
  char r[2048];
  if ( vtkObj == NULL )
  {
    r[0] = '\0';
    sprintf(r, "error retrieving vtk object %s", objName);
    PyErr_SetString(PyRunTimeErr, r);
    return SV_ERROR;
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) )
  {
    r[0] = '\0';
    sprintf(r, "obj %s already exists", objName);
    PyErr_SetString(PyRunTimeErr, r);
    return SV_ERROR;
  }

  pd = new cvPolyData( vtkObj );
  pd->SetName( objName );
  if ( !( gRepository->Register( pd->GetName(), pd ) ) )
  {
    r[0] = '\0';
    sprintf(r, "error registering obj %s in repository", objName);
    PyErr_SetString(PyRunTimeErr, r);
    delete pd;
    return SV_ERROR;
  }

  return Py_BuildValue("s",pd->GetName());

}


// --------------------
// Repos_ExportToVtkCmd
// --------------------

PyObject* Repos_ExportToVtkCmd( PyObject* self, PyObject* args)

{

  char *objName;
  RepositoryDataT type;
  cvRepositoryData *pd;
  vtkObject *vtkObj;

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: objName");
    return SV_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  pd = gRepository->GetObject( objName );
  char r[2048];
  if ( pd == NULL )
  {
    r[0] = '\0';
    sprintf(r, "couldn't find object %s", objName);
    PyErr_SetString(PyRunTimeErr,r);
    return SV_ERROR;
  }

  // Check type (be aware that this implementation is not
  // ideal... what we'd rather be doing here is official RTTI to check
  // that pd is of type cvDataObject... this would be much better than
  // checking for any of the cvDataObject's derived types, since we'll have
  // to remember to update this list if/when more classes are derived
  // from cvDataObject... however RTTI in Sun's WorkShop implementation is
  // not readily cooperating...).

  type = pd->GetType();

  // RTTI version of type check (?):
  //  if ( typeid( pd ) != typeid( cvDataObject ) ) {

  if ( ( type != POLY_DATA_T ) && ( type != STRUCTURED_PTS_T ) &&
       ( type != UNSTRUCTURED_GRID_T ) && ( type != TEMPORALDATASET_T ) )
  {
    r[0] = '\0';
    sprintf(r, "%s not a data object", objName);
    PyErr_SetString(PyRunTimeErr,r);
    return SV_ERROR;
  }

  // vtkTclGetObjectFromPointer takes a vtkObject * and does either:
  //
  //   (i)  finds a pre-existing Tcl binding for this pointer value
  //   (ii) creates a new Tcl command for this pointer, dynamically
  //        figuring out the concrete type via GetClassName()
  //
  // Note that the third arg is a Tcl command function ptr.  It should
  // be something like a vtkPolyDataCommand fn ptr, I think.  It
  // should be OK to pass in NULL, though, because
  // vtkTclGetObjectFromPointer will be able to find the instantiation
  // command based on class name.  That instantiation command
  // (e.g. "vtkPolyData") has a pointer to the object command
  // (e.g. "vtkPolyDataCommand").

  vtkObj = ((cvDataObject *)pd)->GetVtkPtr();
  PyObject* pyVtkObj=vtkPythonUtil::GetObjectFromPointer(vtkObj);

  // The newly-generated object name has already been put in the result.
  return pyVtkObj;

}


// --------------------
// Repos_ImportVtkSpCmd
// --------------------
// Note that for images, vtkImageToStructuredPoints must be applied in
// the interpreter before calling this command.  We could pull that
// conversion inside this function, but there was mention on the vtk
// mailing list a while back about how image caches would be going
// away in vtk 3.0, and so I don't want to code in something that's
// going to become obsolete.

PyObject* Repos_ImportVtkSpCmd( PyObject* self, PyObject* args)

{

  PyObject *vtkName;
  char *objName;
  vtkStructuredPoints *vtkObj;
  cvStrPts *sp;
  if (!PyArg_ParseTuple(args,"Os", &vtkName,&objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: vtkName, objName");
    return SV_ERROR;
  }

  // Look up the named vtk object:
  vtkObj = (vtkStructuredPoints *)vtkPythonUtil::GetPointerFromObject( vtkName,
    "vtkStructuredPoints");
  if ( vtkObj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "error retrieving vtk object ");
    return SV_ERROR;
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) )
  {
    PyErr_SetString(PyRunTimeErr, "obj already exists");
    return SV_ERROR;
  }

  sp = new cvStrPts( vtkObj );
  sp->SetName( objName );
  if ( !( gRepository->Register( sp->GetName(), sp ) ) )
  {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete sp;
    return SV_ERROR;
  }
  return Py_BuildValue("s",sp->GetName());

}


// ----------------------------------
// Repos_ImportVtkUnstructuredGridCmd
// ----------------------------------

PyObject* Repos_ImportVtkUnstructuredGridCmd( PyObject* self, PyObject* args)

{

  PyObject *vtkName;
  char *objName;
  vtkUnstructuredGrid *vtkObj;
  int status;
  cvUnstructuredGrid *sp;

  if (!PyArg_ParseTuple(args,"Os", &vtkName,&objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: vtkName, objName");
    return SV_ERROR;
  }

  // Look up the named vtk object:
  vtkObj = (vtkUnstructuredGrid *)vtkPythonUtil::GetPointerFromObject( vtkName,
    "vtkUnstructuredGrid");
  if ( vtkObj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "error retrieving vtk object ");
    return SV_ERROR;
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) )
  {
    PyErr_SetString(PyRunTimeErr, "obj already exists");
    return SV_ERROR;
  }

  sp = new cvUnstructuredGrid( vtkObj );
  sp->SetName( objName );
  if ( !( gRepository->Register( sp->GetName(), sp ) ) )
  {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete sp;
    return SV_ERROR;
  }
  return Py_BuildValue("s",sp->GetName());

}


// ---------------------
// Repos_ImportVtkImgCmd
// ---------------------

PyObject* Repos_ImportVtkImgCmd( PyObject* self, PyObject* args )

{

  PyObject *vtkName=NULL;
  char *objName=NULL;
  vtkImageData *vtkObj=NULL;
  int status;
  cvStrPts *sp;
  if (!PyArg_ParseTuple(args,"Os", &vtkName,&objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: vtkName, objName");
    return SV_ERROR;
  }

  // Look up the named vtk object:
  vtkObj = (vtkImageData *)vtkPythonUtil::GetPointerFromObject( vtkName,
    "vtkImageData");
  if ( vtkObj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "error retrieving vtk object ");
    return SV_ERROR;
  }
  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) )
  {
    PyErr_SetString(PyRunTimeErr, "obj already exists");
    return SV_ERROR;
  }

  vtkStructuredPoints *mysp = vtkStructuredPoints::New();
  mysp->ShallowCopy(vtkObj);
  // need to shift the origin like what used to be done
  // in vtkImageToStructuredPoints class

  int whole[6];
  int extent[6];
  double *spacing, origin[3];

  //vtkObj->GetWholeExtent(whole);
  //vtkObj->GetExtent(extent);

  // hack job here - vtk-6.0.0 seems to change how to get whole extent
  // so we just assume that we have the whole extent loaded.
  vtkObj->GetExtent(whole);
  spacing = vtkObj->GetSpacing();
  vtkObj->GetOrigin(origin);
  // slide min extent to 0,0,0 (I Hate this !!!!)
  //  this->Translate[0] = whole[0];
  //  this->Translate[1] = whole[2];
  //  this->Translate[2] = whole[4];

  origin[0] += spacing[0] * whole[0];
  origin[1] += spacing[1] * whole[2];
  whole[1] -= whole[0];
  whole[3] -= whole[2];
  whole[0] = 0;
  whole[2] = 0;
  // shift Z origin for 3-D images
  if (whole[4] > 0 && whole[5] > 0) {
    origin[2] += spacing[2] * whole[4];
    whole[5] -= whole[4];
    whole[4] = 0;
  }
  // no longer available in vtk-6.0.0  mysp->SetWholeExtent(whole);
  mysp->SetExtent(whole);
  // Now should Origin and Spacing really be part of information?
  // How about xyx arrays in RectilinearGrid of Points in StructuredGrid?
  mysp->SetOrigin(origin);
  mysp->SetSpacing(spacing);
  sp = new cvStrPts (mysp);
  mysp->Delete();
  sp->SetName( objName );
  if ( !( gRepository->Register( sp->GetName(), sp ) ) )
  {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete sp;
    return SV_ERROR;
  }
  return Py_BuildValue("s",sp->GetName());

}


// -------------
// Repos_SaveCmd
// -------------

PyObject* Repos_SaveCmd( PyObject* self, PyObject* args )

{

  char* filename;
  int saveResult;

  if (!PyArg_ParseTuple(args,"s", &filename))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: filename");
    return SV_ERROR;
  }


  saveResult = gRepository->Save( filename );
  if ( !saveResult )
  {
    PyErr_SetString(PyRunTimeErr, "error saving repository");
    return SV_ERROR;
  }
  return Py_BuildValue("s","repository successfully saved");

}



// -------------
// Repos_LoadCmd
// -------------

PyObject* Repos_LoadCmd( PyObject* self, PyObject* args)

{

  char* filename;
  int loadResult;

  if (!PyArg_ParseTuple(args,"s", &filename))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: filename");
    return SV_ERROR;
  }

  loadResult = gRepository->Load( filename );
  if ( !loadResult )
  {
    PyErr_SetString(PyRunTimeErr, "error loading repository" );
    return SV_ERROR;
  }
  return Py_BuildValue("s","repository successfully load");

}


// -------------------------
// Repos_WriteVtkPolyDataCmd
// -------------------------

// I'm making this command part of the cvRepository package.  Note
// however that it only applies to objects in the cvRepository which are
// based on vtkPolyData.  See vtk/common/vtkSetGet.h for the macro
// used for methods like SetFileName.

PyObject* Repos_WriteVtkPolyDataCmd( PyObject* self, PyObject* args )

{

  char *objName, *ft, *fn;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkPolyData *pd;

  // Define syntax:
  if (!PyArg_ParseTuple(args,"sss", &objName,&ft,&fn))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 3 chars: objName,ft and fn");
    return SV_ERROR;
  }

  // Do work of command:
  char r[1024];
  type = gRepository->GetType( objName );
  if ( type != POLY_DATA_T )
  {
    sprintf(r,"\ &s\" must be of type cvPolyData",objName);
    PyErr_SetString(PyRunTimeErr, r);
    return SV_ERROR;
  }

  obj = gRepository->GetObject( objName );
  switch (type)
  {
  case POLY_DATA_T:
    pd = ((cvPolyData *)obj)->GetVtkPolyData();
    break;
  default:
  PyErr_SetString(PyRunTimeErr, "error in GetVtkPolyData" );
    return SV_ERROR;
    break;
  }

  vtkPolyDataWriter *pdWriter = vtkPolyDataWriter::New();
  pdWriter->SetInputDataObject( pd );
  pdWriter->SetFileName( fn );
  if ( strcmp( ft, "bin" ) == 0 )
  {
    pdWriter->SetFileTypeToBinary();
  }
  else if ( strcmp( ft, "ascii" ) == 0 )
  {
    pdWriter->SetFileTypeToASCII();
  }
  pdWriter->Write();

  pdWriter->Delete();
  return Py_BuildValue("s","success");

}


// ------------------------
// Repos_ReadVtkPolyDataCmd
// ------------------------

PyObject* Repos_ReadVtkPolyDataCmd( PyObject* self, PyObject* args )

{

  char *objName, *fn;
  vtkPolyData *vtkPd;
  cvPolyData *pd;

  if (!PyArg_ParseTuple(args,"ss", &objName,&fn))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: objName and fn");
    return SV_ERROR;
  }

  // Do work of command:

  // Does file exist?
  if ( access( fn, F_OK ) == -1 )
  {
    PyErr_SetString(PyRunTimeErr, "error accessing file ");
    return SV_ERROR;
  }

  vtkPolyDataReader *pdReader = vtkPolyDataReader::New();
  pdReader->SetFileName( fn );

  // Note that it is critical to call Update even for vtk readers.
  pdReader->Update();

  vtkPd = pdReader->GetOutput();
  if ( vtkPd == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "error reading file ");
    pdReader->Delete();
    return SV_ERROR;
  }

  if ( gRepository->Exists( objName ) )
  {
    PyErr_SetString(PyRunTimeErr, "obj already exists");
    pdReader->Delete();
    return SV_ERROR;
  }

  pd = new cvPolyData( vtkPd );
  if ( !( gRepository->Register( objName, pd ) ) )
  {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    pdReader->Delete();
    delete pd;
    return SV_ERROR;
  }

  PyObject* n = Py_BuildValue("s",pd->GetName());
  pdReader->Delete();
  return n;

}


// ---------------------------------
// Repos_WriteVtkStructuredPointsCmd
// ---------------------------------
PyObject* Repos_WriteVtkStructuredPointsCmd( PyObject* self, PyObject* args)

{

  char *objName, *ft, *fn;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkStructuredPoints *sp;
  if (!PyArg_ParseTuple(args,"sss", &objName,&ft,&fn))
  {
    PyErr_SetString(PyRunTimeErr,
      "Could not import 3 chars: objName,ft-type and fn-file");
    return SV_ERROR;
  }

  // Do work of command:
  char r[1024];
  type = gRepository->GetType( objName );
  if ( type != STRUCTURED_PTS_T )
  {
    sprintf(r,"\"%s\" must be of a structured points type",objName);
    PyErr_SetString(PyRunTimeErr, r);
    return SV_ERROR;
  }

  obj = gRepository->GetObject( objName );
  switch (type)
  {
  case STRUCTURED_PTS_T:
    sp = ((cvStrPts *)obj)->GetVtkStructuredPoints();
    break;
  default:
    PyErr_SetString(PyRunTimeErr, "error in GetVtkStructuredPoints" );
    return SV_ERROR;
    break;
  }

  vtkStructuredPointsWriter *spWriter = vtkStructuredPointsWriter::New();
  spWriter->SetInputDataObject( sp );
  spWriter->SetFileName( fn );
  if ( strcmp( ft, "bin" ) == 0 )
  {
    spWriter->SetFileTypeToBinary();
  }
  else if ( strcmp( ft, "ascii" ) == 0 )
  {
    spWriter->SetFileTypeToASCII();
  }
  spWriter->Write();

  spWriter->Delete();

  return Py_BuildValue("s","success");

}


// ---------------------------------
// Repos_WriteVtkUnstructuredGridCmd
// ---------------------------------
PyObject* Repos_WriteVtkUnstructuredGridCmd( PyObject* self, PyObject* args)

{

  char *objName, *ft, *fn;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkUnstructuredGrid *sp;

  // Define syntax:
  if (!PyArg_ParseTuple(args,"sss", &objName,&ft,&fn))
  {
    PyErr_SetString(PyRunTimeErr,
      "Could not import 3 chars: objName,ft-type and fn-file");
    return SV_ERROR;
  }

  // Do work of command:
  type = gRepository->GetType( objName );
  if ( type != UNSTRUCTURED_GRID_T )
  {
    PyErr_SetString(PyRunTimeErr, "\"%s\" must be of a structured points type");
    return SV_ERROR;
  }

  obj = gRepository->GetObject( objName );
  switch (type)
  {
  case UNSTRUCTURED_GRID_T:
    sp = ((cvUnstructuredGrid*)obj)->GetVtkUnstructuredGrid();
    break;
  default:
  PyErr_SetString(PyRunTimeErr, "error in GetVtkUnstructuredGrid" );
    return SV_ERROR;
    break;
  }

  vtkUnstructuredGridWriter *spWriter = vtkUnstructuredGridWriter::New();
  spWriter->SetInputDataObject( sp );
  spWriter->SetFileName( fn );
  if ( strcmp( ft, "bin" ) == 0 )
  {
    spWriter->SetFileTypeToBinary();
  }
  else if ( strcmp( ft, "ascii" ) == 0 )
  {
    spWriter->SetFileTypeToASCII();
  }
  spWriter->Write();

  spWriter->Delete();

  return Py_BuildValue("s","success");

}


// ---------------------
// Repos_GetLabelKeysCmd
// ---------------------

PyObject* Repos_GetLabelKeysCmd( PyObject* self, PyObject* args)

{

  char *objName;
  cvRepositoryData *obj;
  int numKeys, i;
  char **keys;

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: objName");
    return SV_ERROR;
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    return SV_ERROR;
  }

  obj->GetLabelKeys( &numKeys, &keys );

  PyObject *pylist=PyList_New(0);
  for (i = 0; i < numKeys; i++)
  {
    PyObject* pyKeys = PyString_FromString(keys[i]);
    PyList_Append( pylist,  pyKeys );
  }

  return pylist;

}


// -----------------
// Repos_GetLabelCmd
// -----------------

PyObject* Repos_GetLabelCmd( PyObject* self, PyObject* args)

{

  char *objName;
  cvRepositoryData *obj;
  char *key, *value;

  if (!PyArg_ParseTuple(args,"ss", &objName,&key))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: objName,key");
    return SV_ERROR;
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return SV_ERROR;
  }

  if ( ! obj->GetLabel( key, &value ) )
  {
    PyErr_SetString(PyRunTimeErr, "key not found" );
    return SV_ERROR;
  }

  return Py_BuildValue("s",value);

}


// -----------------
// Repos_SetLabelCmd
// -----------------
PyObject* Repos_SetLabelCmd( PyObject* self, PyObject* args)

{

  char *objName;
  cvRepositoryData *obj;
  char *key, *value;

  if (!PyArg_ParseTuple(args,"sss", &objName,&key,&value))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 3 chars: objName,key and value");
    return SV_ERROR;
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    return SV_ERROR;
  }

  if ( ! obj->SetLabel( key, value ) )
  {
    if ( !obj->IsLabelPresent( key ) )
    {
      PyErr_SetString(PyRunTimeErr, "key already in use");
      return SV_ERROR;
    }
    else
    {
      PyErr_SetString(PyRunTimeErr, "error setting label ");
      return SV_ERROR;
    }
  }

  return Py_BuildValue("s","success");

}


// -------------------
// Repos_ClearLabelCmd
// -------------------

PyObject* Repos_ClearLabelCmd( PyObject* self, PyObject* args)

{

  char *objName;
  cvRepositoryData *obj;
  char *key;

  if (!PyArg_ParseTuple(args,"ss", &objName,&key))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: objName,key");
    return SV_ERROR;
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object" );
    return SV_ERROR;
  }

  if ( ! obj->IsLabelPresent( key ) )
  {
    PyErr_SetString(PyRunTimeErr, "key not found");
    return SV_ERROR;
  }

  obj->ClearLabel( key );

  return Py_BuildValue("s","success");

}
