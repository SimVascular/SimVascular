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
#include "Python.h"
#include "SimVascular_python.h"


#include "sv_repos_init_py.h"
#include "sv_Repository.h"
#include "sv_PolyData.h"
#include "sv_StrPts.h"
#include "sv_UnstructuredGrid.h"
#include "sv_arg.h"
#include "sv_VTK.h"
#include "vtkTclUtil.h"
#include "vtkPythonUtil.h"
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLPolyDataWriter.h>

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// The global cvRepository object should be allocated in the file where
// main() lives.  ... Why?

#include "sv2_globals.h"
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

PyObject* Repos_ReadVtkXMLPolyDataCmd( PyObject* self, PyObject* args );

PyObject* Repos_WriteVtkXMLPolyDataCmd( PyObject* self, PyObject* args );

PyObject*  Repos_WriteVtkStructuredPointsCmd( PyObject* self, PyObject* args);

PyObject*  Repos_WriteVtkUnstructuredGridCmd( PyObject* self, PyObject* args);

PyObject*  Repos_SetStringCmd( PyObject* self, PyObject* args);

PyObject* Repos_GetStringCmd( PyObject* self, PyObject* args);

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyRepository(void);
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyRepository(void);
#endif

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

    {"List", Repos_ListCmd, METH_NOARGS,NULL},
    {"Exists", Repos_ExistsCmd, METH_VARARGS,NULL},
    {"Delete", Repos_DeleteCmd, METH_VARARGS,NULL},
    {"Type", Repos_TypeCmd, METH_VARARGS,NULL},
    {"ImportVtkPd", Repos_ImportVtkPdCmd, METH_VARARGS,NULL},
    {"ExportToVtk", Repos_ExportToVtkCmd, METH_VARARGS,NULL},
    {"ImportVtkSp", Repos_ImportVtkSpCmd, METH_VARARGS,NULL},
    {"ImportVtkImg", Repos_ImportVtkImgCmd, METH_VARARGS,NULL},
    {"ImportVtkUnstructuredGrid", Repos_ImportVtkUnstructuredGridCmd,
      METH_VARARGS,NULL},
    {"Save", Repos_SaveCmd, METH_VARARGS,NULL},
    {"Load", Repos_LoadCmd, METH_VARARGS,NULL},
    {"WriteVtkPolyData", Repos_WriteVtkPolyDataCmd, METH_VARARGS,NULL},
    {"ReadVtkPolyData", Repos_ReadVtkPolyDataCmd, METH_VARARGS,NULL},
    {"ReadXMLPolyData",Repos_ReadVtkXMLPolyDataCmd,METH_VARARGS,NULL},
    {"WriteXMLPolyData",Repos_WriteVtkXMLPolyDataCmd,METH_VARARGS,NULL},
    {"WriteVtkStructuredPoints", Repos_WriteVtkStructuredPointsCmd,
     METH_VARARGS,NULL},
    {"WriteVtkUnstructuredGrid", Repos_WriteVtkUnstructuredGridCmd,
     METH_VARARGS,NULL},
    {"GetLabelKeys", Repos_GetLabelKeysCmd, METH_VARARGS,NULL},
    {"GetLabel", Repos_GetLabelCmd, METH_VARARGS,NULL},
    {"SetLabel", Repos_SetLabelCmd, METH_VARARGS,NULL},
    {"ClearLabel", Repos_ClearLabelCmd, METH_VARARGS,NULL},
    {"Setstring", Repos_SetStringCmd, METH_VARARGS,NULL},
    {"Getstring", Repos_GetStringCmd, METH_VARARGS,NULL},
    {NULL, NULL,0,NULL},

};
#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyRepositorymodule = {
   PyModuleDef_HEAD_INIT,
   "pyRepository",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyRepository_methods
};
#endif

#if PYTHON_MAJOR_VERSION == 2
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

#endif
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyRepository(void)
{
      PyObject *pyRepo;
  gRepository = new cvRepository();
  if ( gRepository == NULL ) {
    fprintf( stderr, "error allocating gRepository\n" );

    return SV_PYTHON_ERROR;
  }

  pyRepo = PyModule_Create(& pyRepositorymodule);
  PyRunTimeErr = PyErr_NewException("pyRepository.error",NULL,NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(pyRepo,"error",PyRunTimeErr);
  return pyRepo;
}
#endif
int Repos_pyInit()

{

#if PYTHON_MAJOR_VERSION == 2
  initpyRepository();
#elif PYTHON_MAJOR_VERSION == 3
  PyInit_pyRepository();
#endif
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
  
  }

  // Retrieve source object:
  pd = gRepository->GetObject( stringPd );
  if ( pd == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object stringPd");
    
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
    
  }

  // Retrieve source object:
  pd = gRepository->GetObject( stringPd );
  if ( pd == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object stringPd");
    
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
    
  }

  exists = gRepository->Exists( name );

  return Py_BuildValue("N",PyBool_FromLong(exists));

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
    
  }

  // Do work of command:
  char r[2048];
  exists = gRepository->Exists( objName );
  if ( ! exists ) {
    r[0] = '\0';
    sprintf(r,"object %s not in repository",objName);
    PyErr_SetString(PyRunTimeErr, r);
    
  }

  obj_t = gRepository->GetType( objName );
  //if ( obj_t == SOLID_MODEL_T || obj_t == MESH_T || obj_t == ADAPTOR_T) {
  //  return Tcl_VarEval( interp, "rename ", objName, " {}", (char *)NULL );
  //} else {
    unreg_status = gRepository->UnRegister( objName );
  //}

  if ( ! unreg_status )
  {
    r[0] = '\0';
    sprintf(r,"error deleting object %s", objName);
    PyErr_SetString(PyRunTimeErr, r);
    
  }
  return SV_PYTHON_OK;

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
    
  }

  char r[2048];
  if ( ! gRepository->Exists( name ) )
  {
    r[0] = '\0';
    sprintf(r, "object %s not found", name);
    PyErr_SetString(PyRunTimeErr,r);
    
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
    
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) )
  {
    r[0] = '\0';
    sprintf(r, "obj %s already exists", objName);
    PyErr_SetString(PyRunTimeErr, r);
    
  }

  pd = new cvPolyData( vtkObj );
  pd->SetName( objName );
  if ( !( gRepository->Register( pd->GetName(), pd ) ) )
  {
    r[0] = '\0';
    sprintf(r, "error registering obj %s in repository", objName);
    PyErr_SetString(PyRunTimeErr, r);
    delete pd;
    
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
    
  }

  // Look up the named vtk object:
  vtkObj = (vtkStructuredPoints *)vtkPythonUtil::GetPointerFromObject( vtkName,
    "vtkStructuredPoints");
  if ( vtkObj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "error retrieving vtk object ");
    
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) )
  {
    PyErr_SetString(PyRunTimeErr, "obj already exists");
    
  }

  sp = new cvStrPts( vtkObj );
  sp->SetName( objName );
  if ( !( gRepository->Register( sp->GetName(), sp ) ) )
  {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete sp;
    
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
    
  }

  // Look up the named vtk object:
  vtkObj = (vtkUnstructuredGrid *)vtkPythonUtil::GetPointerFromObject( vtkName,
    "vtkUnstructuredGrid");
  if ( vtkObj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "error retrieving vtk object ");
    
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) )
  {
    PyErr_SetString(PyRunTimeErr, "obj already exists");
    
  }

  sp = new cvUnstructuredGrid( vtkObj );
  sp->SetName( objName );
  if ( !( gRepository->Register( sp->GetName(), sp ) ) )
  {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete sp;
    
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
    
  }

  // Look up the named vtk object:
  vtkObj = (vtkImageData *)vtkPythonUtil::GetPointerFromObject( vtkName,
    "vtkImageData");
  if ( vtkObj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "error retrieving vtk object ");
    
  }
  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) )
  {
    PyErr_SetString(PyRunTimeErr, "obj already exists");
    
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
    
  }


  saveResult = gRepository->Save( filename );
  if ( !saveResult )
  {
    PyErr_SetString(PyRunTimeErr, "error saving repository");
    
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
    
  }

  loadResult = gRepository->Load( filename );
  if ( !loadResult )
  {
    PyErr_SetString(PyRunTimeErr, "error loading repository" );
    
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
    
  }

  // Do work of command:
  char r[1024];
  type = gRepository->GetType( objName );
  if ( type != POLY_DATA_T )
  {
    sprintf(r,"\"&s\" must be of type cvPolyData",objName);
    PyErr_SetString(PyRunTimeErr, r);
    
  }

  obj = gRepository->GetObject( objName );
  switch (type)
  {
  case POLY_DATA_T:
    pd = ((cvPolyData *)obj)->GetVtkPolyData();
    break;
  default:
  PyErr_SetString(PyRunTimeErr, "error in GetVtkPolyData" );
    
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
  return SV_PYTHON_OK;

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
    
  }

  // Do work of command:

  vtkPolyDataReader *pdReader = vtkPolyDataReader::New();
  pdReader->SetFileName( fn );

  // Note that it is critical to call Update even for vtk readers.
  pdReader->Update();

  vtkPd = pdReader->GetOutput();
  if ( vtkPd == NULL ||vtkPd->GetNumberOfPolys()==0 )
  {
    PyErr_SetString(PyRunTimeErr, "error reading file ");
    pdReader->Delete();
    
  }

  if ( gRepository->Exists( objName ) )
  {
    PyErr_SetString(PyRunTimeErr, "obj already exists");
    pdReader->Delete();
    
  }

  pd = new cvPolyData( vtkPd );
  if ( !( gRepository->Register( objName, pd ) ) )
  {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    pdReader->Delete();
    delete pd;
    
  }

  PyObject* n = Py_BuildValue("s",pd->GetName());
  pdReader->Delete();
  return n;

}

// ------------------------
// Repos_ReadVtkXMLPolyDataCmd
// ------------------------

PyObject* Repos_ReadVtkXMLPolyDataCmd( PyObject* self, PyObject* args )

{

  char *objName, *fn;
  vtkPolyData *vtkPd;
  cvPolyData *pd;

  if (!PyArg_ParseTuple(args,"ss", &objName,&fn))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: objName and fn");
    
  }

  // Do work of command:

  vtkXMLPolyDataReader *pdReader = vtkXMLPolyDataReader::New();
  pdReader->SetFileName( fn );

  // Note that it is critical to call Update even for vtk readers.
  pdReader->Update();

  vtkPd = pdReader->GetOutput();
  if ( vtkPd == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "error reading file ");
    pdReader->Delete();
    
  }

  if ( gRepository->Exists( objName ) )
  {
    PyErr_SetString(PyRunTimeErr, "obj already exists");
    pdReader->Delete();
    
  }

  pd = new cvPolyData( vtkPd );
  if ( !( gRepository->Register( objName, pd ) ) )
  {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    pdReader->Delete();
    delete pd;
    
  }

  PyObject* n = Py_BuildValue("s",pd->GetName());
  pdReader->Delete();
  return n;

}



// ---------------------------------
// Repos_WriteVtkXMLPolyDataCmd
// ---------------------------------
PyObject* Repos_WriteVtkXMLPolyDataCmd( PyObject* self, PyObject* args)

{

  char *objName, *fn;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkStructuredPoints *sp;
  if (!PyArg_ParseTuple(args,"ss", &objName,&fn))
  {
    PyErr_SetString(PyRunTimeErr,
      "Could not import 2 chars: objName, and filename");
    return NULL;
  }

  // Do work of command:
  char r[1024];
  type = gRepository->GetType( objName );
  if ( type != POLY_DATA_T )
  {
    sprintf(r,"\"%s\" must be of a PolyData type",objName);
    PyErr_SetString(PyRunTimeErr, r);
    return NULL;
  }

  obj = gRepository->GetObject( objName );
  sp = ((cvStrPts *)obj)->GetVtkStructuredPoints();

  vtkXMLPolyDataWriter *spWriter = vtkXMLPolyDataWriter::New();
  spWriter->SetInputDataObject( sp );
  spWriter->SetFileName( fn );
  spWriter->Write();
  spWriter->Delete();

  return SV_PYTHON_OK;

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
    
  }

  // Do work of command:
  char r[1024];
  type = gRepository->GetType( objName );
  if ( type != STRUCTURED_PTS_T )
  {
    sprintf(r,"\"%s\" must be of a structured points type",objName);
    PyErr_SetString(PyRunTimeErr, r);
    
  }

  obj = gRepository->GetObject( objName );
  switch (type)
  {
  case STRUCTURED_PTS_T:
    sp = ((cvStrPts *)obj)->GetVtkStructuredPoints();
    break;
  default:
    PyErr_SetString(PyRunTimeErr, "error in GetVtkStructuredPoints" );
    
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

  return SV_PYTHON_OK;

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
    
  }

  // Do work of command:
  type = gRepository->GetType( objName );
  if ( type != UNSTRUCTURED_GRID_T )
  {
    PyErr_SetString(PyRunTimeErr, "\"%s\" must be of a structured points type");
    
  }

  obj = gRepository->GetObject( objName );
  switch (type)
  {
  case UNSTRUCTURED_GRID_T:
    sp = ((cvUnstructuredGrid*)obj)->GetVtkUnstructuredGrid();
    break;
  default:
  PyErr_SetString(PyRunTimeErr, "error in GetVtkUnstructuredGrid" );
    
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

  return SV_PYTHON_OK;

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
    
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    
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
    
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    
  }

  if ( ! obj->GetLabel( key, &value ) )
  {
    PyErr_SetString(PyRunTimeErr, "key not found" );
    
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
    
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    
  }

  if ( ! obj->SetLabel( key, value ) )
  {
    if ( !obj->IsLabelPresent( key ) )
    {
      PyErr_SetString(PyRunTimeErr, "key already in use");
      
    }
    else
    {
      PyErr_SetString(PyRunTimeErr, "error setting label ");
      
    }
  }

  return SV_PYTHON_OK;

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
    
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL )
  {
    PyErr_SetString(PyRunTimeErr, "couldn't find object" );
    
  }

  if ( ! obj->IsLabelPresent( key ) )
  {
    PyErr_SetString(PyRunTimeErr, "key not found");
    
  }

  obj->ClearLabel( key );

  return SV_PYTHON_OK;

}
