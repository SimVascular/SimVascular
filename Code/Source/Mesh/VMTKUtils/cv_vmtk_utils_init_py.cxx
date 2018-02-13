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

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cvRepositoryData.h"
#include "cvPolyData.h"
#include "cv_vmtk_utils_init.h"
#include "cv_vmtk_utils.h"
#include "cvSolidModel.h"
#include "cv_vtk_utils.h"
#include "Python.h"
#include "vtkSmartPointer.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"


// Prototypes:
// -----------
#ifdef SV_USE_VMTK
PyObject* PyRunTimeErr;
PyMODINIT_FUNC initpyVMTKUtils();

PyObject* Geom_CenterlinesCmd( PyObject* self, PyObject* args);

PyObject* Geom_DistanceToCenterlinesCmd( PyObject* self, PyObject* args);

PyObject* Geom_GroupPolyDataCmd( PyObject* self, PyObject* args);

PyObject* Geom_SeparateCenterlinesCmd( PyObject* self, PyObject* args);

PyObject* Geom_MergeCenterlinesCmd( PyObject* self, PyObject* args);

PyObject* Geom_CapCmd( PyObject* self, PyObject* args);

PyObject* Geom_CapWIdsCmd( PyObject* self, PyObject* args);

PyObject* Geom_MapAndCorrectIdsCmd( PyObject* self, PyObject* args);
#endif

// Helper functions
// ----------------


// ---------
// Vmtkutils_Init
// ---------

int Vmtkutils_pyInit()
{
  initpyVMTKUtils();
  return Py_OK;
}

//-----------------
//VMTKUtils_methods
//-----------------
PyMethodDef VMTKUtils_methods[]=
{
#ifdef SV_USE_VMTK
  { "geom_centerlines", Geom_CenterlinesCmd, METH_VARARGS,NULL},
  { "geom_grouppolydata", Geom_GroupPolyDataCmd, METH_VARARGS,NULL},
  { "geom_distancetocenterlines", Geom_DistanceToCenterlinesCmd, METH_VARARGS,NULL},
  { "geom_separatecenterlines", Geom_SeparateCenterlinesCmd, METH_VARARGS,NULL},
  { "geom_mergecenterlines", Geom_MergeCenterlinesCmd, METH_VARARGS,NULL},
  { "geom_cap", Geom_CapCmd, METH_VARARGS,NULL},
  { "geom_cap_with_ids", Geom_CapWIdsCmd, METH_VARARGS,NULL},
  { "geom_mapandcorrectids", Geom_MapAndCorrectIdsCmd, METH_VARARGS,NULL},
#endif
  {NULL,NULL}
};

//------------------
//initpyVMTKUtils
//------------------
PyMODINIT_FUNC initpyVMTKUtils()
{
  PyObject* pythonC;
  pythonC=Py_InitModule("pyVMTKUtils",VMTKUtils_methods);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error initializing pyVMTKUtils.\n");
    return;
  }
  PyRunTimeErr=PyErr_NewException("pyVMTKUtils.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  return;
}

//--------------------
//Geom_CenterlinesCmd
//--------------------
#ifdef SV_USE_VMTK
PyObject* Geom_CenterlinesCmd( PyObject* self, PyObject* args)
{
  char *usage;
  PyObject* sourceList;
  PyObject* targetList;
  char *linesName;
  char *voronoiName;
  char *geomName;
  cvRepositoryData *geomSrc;
  cvRepositoryData *linesDst = NULL;
  cvRepositoryData *voronoiDst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sOOss",&geomName,&sourceList,&targetList,
	&linesName, &voronoiName))
  {
    PyErr_SetString(PyRunTimeErr,
	"Could not import three chars and two list, geomName, sourceList,"
	"targetList, linesName, voronoiName");
    return Py_ERROR;
  }

  // Retrieve source object:
  geomSrc = gRepository->GetObject( geomName );
  if ( geomSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object" );
    return Py_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, " obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( linesName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  if ( gRepository->Exists( voronoiName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  int nsources = PyList_Size(sourceList);
  int ntargets = PyList_Size(targetList);
  if (nsources==0||ntargets==0)
  {
    return Py_BuildValue("s","success");
  }

  int *sources = new int[nsources];
  int *targets = new int[ntargets];

  for (int i=0;i<nsources;i++)
  {
    sources[i]=PyLong_AsLong(PyList_GetItem(sourceList,i));
  }
  for (int j=0;j<ntargets;j++)
  {
    targets[j]=PyLong_AsLong(PyList_GetItem(targetList,j));
  }
  // Do work of command:

  if ( sys_geom_centerlines( (cvPolyData*)geomSrc, sources, nsources, targets, ntargets, (cvPolyData**)(&linesDst), (cvPolyData**)(&voronoiDst))
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr,"error creating centerlines");
    return Py_ERROR;
  }

  delete [] sources;
  delete [] targets;

  if ( !( gRepository->Register( linesName, linesDst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete linesDst;
    delete voronoiDst;
    return Py_ERROR;
  }

  if ( !( gRepository->Register( voronoiName, voronoiDst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete linesDst;
    delete voronoiDst;
    return Py_ERROR;
  }


  return Py_BuildValue("s",linesDst->GetName());
}

//------------------
//Geom_GroupPolyDataCmd
//------------------
PyObject* Geom_GroupPolyDataCmd( PyObject* self, PyObject* args)
{
  char *usage;
  char *geomName;
  char *linesName;
  char *groupedName;
  cvRepositoryData *geomSrc;
  cvRepositoryData *linesSrc;
  cvRepositoryData *groupedDst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sss",&geomName,
	&linesName, &groupedName))
  {
    PyErr_SetString(PyRunTimeErr,
	"Could not import three chars, geomName,linesName, groupedName");
    return Py_ERROR;
  }
  // Retrieve source object:
  geomSrc = gRepository->GetObject( geomName );
  if ( geomSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Retrieve source object:
  linesSrc = gRepository->GetObject( linesName );
  if ( linesSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }

  type = linesSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Do work of command:

  if ( sys_geom_grouppolydata( (cvPolyData*)geomSrc, (cvPolyData*)linesSrc, (cvPolyData**)(&groupedDst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error getting grouped polydata" );
    return Py_ERROR;
  }

  if ( !( gRepository->Register( groupedName, groupedDst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete groupedDst;
    return Py_ERROR;
  }


  return Py_BuildValue("s",groupedDst->GetName()) ;
}

PyObject* Geom_DistanceToCenterlinesCmd( PyObject* self, PyObject* args)
{
  char *usage;
  char *geomName;
  char *linesName;
  char *distanceName;
  cvRepositoryData *geomSrc;
  cvRepositoryData *linesSrc;
  cvRepositoryData *distanceDst = NULL;
  RepositoryDataT type;
  if (!PyArg_ParseTuple(args,"sss",&geomName,
	&linesName, &distanceName))
  {
    PyErr_SetString(PyRunTimeErr,
	"Could not import three chars, geomName,linesName, distanceName");
    return Py_ERROR;
  }

  // Retrieve source object:
  geomSrc = gRepository->GetObject( geomName );
  if ( geomSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object" );
    return Py_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Retrieve source object:
  linesSrc = gRepository->GetObject( linesName );
  if ( linesSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }

  type = linesSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Do work of command:

  if ( sys_geom_distancetocenterlines( (cvPolyData*)geomSrc, (cvPolyData*)linesSrc, (cvPolyData**)(&distanceDst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error getting distance to centerlines" );
    return Py_ERROR;
  }

  if ( !( gRepository->Register( distanceName, distanceDst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete distanceDst;
    return Py_ERROR;
  }


  return Py_BuildValue("s", distanceDst->GetName()) ;
}

PyObject* Geom_SeparateCenterlinesCmd( PyObject* self, PyObject* args)
{
  char *usage;
  char *linesName;
  char *separateName;
  cvRepositoryData *linesSrc;
  cvRepositoryData *separateDst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss",&linesName, &separateName))
  {
    PyErr_SetString(PyRunTimeErr,
	"Could not import two chars,linesName, separateName");
    return Py_ERROR;
  }
  // Retrieve source object:
  linesSrc = gRepository->GetObject( linesName );
  if ( linesSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    return Py_ERROR;
  }

  type = linesSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Do work of command:

  if ( sys_geom_separatecenterlines( (cvPolyData*)linesSrc, (cvPolyData**)(&separateDst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error grouping centerlines" );
    return Py_ERROR;
  }

  if ( !( gRepository->Register( separateName, separateDst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete separateDst;
    return Py_ERROR;
  }

  return Py_BuildValue("s",separateDst->GetName()) ;
}

PyObject* Geom_MergeCenterlinesCmd( PyObject* self, PyObject* args)
{
  char *usage;
  char *linesName;
  char *mergeName;
  int mergeblanked = 1;
  cvRepositoryData *linesSrc;
  cvRepositoryData *mergeDst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ssi",&linesName,
	&mergeName, &mergeblanked))
  {
    PyErr_SetString(PyRunTimeErr,
	"Could not import two chars and one int, linesName,mergeName, mergeblanked");
    return Py_ERROR;
  }
  // Retrieve source object:
  linesSrc = gRepository->GetObject( linesName );
  if ( linesSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    return Py_ERROR;
  }

  type = linesSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Do work of command:

  if ( sys_geom_mergecenterlines( (cvPolyData*)linesSrc, mergeblanked, (cvPolyData**)(&mergeDst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error merging centerlines" );
    return Py_ERROR;
  }

  if ( !( gRepository->Register( mergeName, mergeDst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete mergeDst;
    return Py_ERROR;
  }


  return Py_BuildValue("s", mergeDst->GetName());
}


PyObject* Geom_CapCmd( PyObject* self, PyObject* args)
{
  int numIds;
  int *ids;
  int captype;
  char *usage;
  char *cappedName;
  char *geomName;
  char idstring[256];
  cvRepositoryData *geomSrc;
  cvRepositoryData *cappedDst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sss",&geomName,
	&cappedName, &captype))
  {
    PyErr_SetString(PyRunTimeErr,
	"Could not import three chars, geomName,cappedName, captype");
    return Py_ERROR;
  }
  // Retrieve source object:
  geomSrc = gRepository->GetObject( geomName );
  if ( geomSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    return Py_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( cappedName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    return Py_ERROR;
  }

  // Do work of command:

  if ( sys_geom_cap( (cvPolyData*)geomSrc, (cvPolyData**)(&cappedDst), &numIds,&ids,captype )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr,"error capping model" );
    return Py_ERROR;
  }

  if ( !( gRepository->Register( cappedName, cappedDst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete cappedDst;
    return Py_ERROR;
  }

//  Tcl_SetResult( interp, cappedDst->GetName() );

  if (numIds == 0)
  {
    PyErr_SetString(PyRunTimeErr, "No Ids Found" );
    return Py_ERROR;
  }
  PyObject* pyList=PyList_New(numIds);
  for (int i = 0; i < numIds; i++) {
	sprintf(idstring, "%i", ids[i]);
    PyList_SetItem(pyList,i,PyString_FromFormat(idstring));
	idstring[0]='\n';
  }
  delete [] ids;

  return pyList;
}

PyObject* Geom_CapWIdsCmd( PyObject* self, PyObject* args)
{
  int fillId;
  char *usage;
  char *cappedName;
  char *geomName;
  int num_filled = 0;
  int filltype = 0;
  cvRepositoryData *geomSrc;
  cvRepositoryData *cappedDst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ssii",&geomName,
	&cappedName, &fillId,&filltype))
  {
    PyErr_SetString(PyRunTimeErr,
	"Could not import two chars and two ints, geomName,cappedName, fillId,filltype");
    return Py_ERROR;
  }
  // Retrieve source object:
  geomSrc = gRepository->GetObject( geomName );
  if ( geomSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,"obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( cappedName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Do work of command:

  if ( sys_geom_cap_with_ids( (cvPolyData*)geomSrc, (cvPolyData**)(&cappedDst)
	,fillId,num_filled,filltype)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error capping model" );
    return Py_ERROR;
  }

  if ( !( gRepository->Register( cappedName, cappedDst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete cappedDst;
    return Py_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d",num_filled);

  return Py_BuildValue("s",rtnstr);
}

PyObject* Geom_MapAndCorrectIdsCmd( PyObject* self, PyObject* args)
{
  char *usage;
  char *originalName;
  char *newName;
  char *resultName;
  char *originalArray;
  char *newArray;
  cvRepositoryData *geomSrc;
  cvRepositoryData *geomNew;
  cvRepositoryData *geomDst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sssss",&originalName,
	&newName, &resultName,&originalArray,&newArray))
  {
    PyErr_SetString(PyRunTimeErr,
	"Could not import five chars, originalName,newName, resultName"
	"originalArray, newArray");
    return Py_ERROR;
  }
  // Retrieve source object:
  geomSrc = gRepository->GetObject( originalName );
  if ( geomSrc == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    return Py_ERROR;
  }

  // Retrieve source object:
  geomNew = gRepository->GetObject( newName );
  if ( geomNew == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }

  type = geomSrc->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    return Py_ERROR;
  }

  type = geomNew->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    return Py_ERROR;
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Do work of command:

  if ( sys_geom_mapandcorrectids( (cvPolyData*)geomSrc, (cvPolyData*)geomNew, (cvPolyData**)(&geomDst), originalArray,newArray )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error correcting ids" );
    return Py_ERROR;
  }

  if ( !( gRepository->Register( resultName, geomDst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geomDst;
    return Py_ERROR;
  }


  return Py_BuildValue("s",geomDst->GetName()) ;
}
#endif
