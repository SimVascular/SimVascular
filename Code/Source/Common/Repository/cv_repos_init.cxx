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

#include "cv_repos_init.h"
#include "cvRepository.h"
#include "cvPolyData.h"
#include "cvStrPts.h"
#include "cvUnstructuredGrid.h"
#include "cv_arg.h"
#include "cvVTK.h"
#include "vtkTclUtil.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// The global cvRepository object should be allocated in the file where
// main() lives.  ... Why?

#include "cv_globals.h"

// Prototypes:

int Repos_ListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int Repos_ExistsCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

int Repos_DeleteCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] );

int Repos_TypeCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int Repos_ImportVtkPdCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int Repos_ImportVtkSpCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int Repos_ImportVtkImgCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Repos_ImportVtkUnstructuredGridCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

int Repos_ExportToVtkCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int Repos_SaveCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int Repos_LoadCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );

int Repos_WriteVtkPolyDataCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] );

int Repos_ReadVtkPolyDataCmd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

int Repos_WriteVtkStructuredPointsCmd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] );

int Repos_WriteVtkUnstructuredGridCmd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] );


  // Label-related methods
  // ---------------------

static int Repos_GetLabelKeysCmd( ClientData clientData, Tcl_Interp *interp,
				  int argc, CONST84 char *argv[] );

static int Repos_GetLabelCmd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

static int Repos_SetLabelCmd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] );

static int Repos_ClearLabelCmd( ClientData clientData, Tcl_Interp *interp,
				int argc, CONST84 char *argv[] );


// ----------
// Repos_Init
// ----------

// The action-oriented cvRepository Tcl-level commands which we might
// want:
//   - repos_list
//   - repos_exists <name>
//   - repos_delete <name>
//   - repos_type <name>
//   - repos_importFromVtk -src <VtkTclSrc> -obj <name>
//   - repos_save <filename>
//   - repos_load <filename>

int Repos_Init( Tcl_Interp *interp )
{
  // Create the repository in a global space

  gRepository = new cvRepository();
  if ( gRepository == NULL ) {
    fprintf( stderr, "error allocating gRepository\n" );
    return TCL_ERROR;
  }

  Tcl_CreateCommand( interp, "repos_list", Repos_ListCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_exists", Repos_ExistsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_delete", Repos_DeleteCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_type", Repos_TypeCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_importVtkPd", Repos_ImportVtkPdCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_exportToVtk", Repos_ExportToVtkCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_importVtkSp", Repos_ImportVtkSpCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_importVtkImg", Repos_ImportVtkImgCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_importVtkUnstructuredGrid", Repos_ImportVtkUnstructuredGridCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_save", Repos_SaveCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_load", Repos_LoadCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_writeVtkPolyData",
		     Repos_WriteVtkPolyDataCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_readVtkPolyData",
		     Repos_ReadVtkPolyDataCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_writeVtkStructuredPoints",
		     Repos_WriteVtkStructuredPointsCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_writeVtkUnstructuredGrid",
		     Repos_WriteVtkUnstructuredGridCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_getLabelKeys", Repos_GetLabelKeysCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_getLabel", Repos_GetLabelCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_setLabel", Repos_SetLabelCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
  Tcl_CreateCommand( interp, "repos_clearLabel", Repos_ClearLabelCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

  return TCL_OK;
}

// -------------
// Repos_ListCmd
// -------------

int Repos_ListCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *name;

  if ( argc != 1 ) {
    Tcl_SetResult( interp, "usage: repos_list", TCL_STATIC );
    return TCL_ERROR;
  }
  gRepository->InitIterator();
  while ( name = gRepository->GetNextName() ) {
    Tcl_AppendElement( interp, name );
  }
  return TCL_OK;
}


// ---------------
// Repos_ExistsCmd
// ---------------

int Repos_ExistsCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  char *name;
  int exists;
  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &name, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1, table_sz, arg_table )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  exists = gRepository->Exists( name );

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%d", exists );
  Tcl_SetResult( interp, rtnstr, TCL_VOLATILE ); 

  return TCL_OK;
}


// ---------------
// Repos_DeleteCmd
// ---------------

int Repos_DeleteCmd( ClientData clientData, Tcl_Interp *interp,
		     int argc, CONST84 char *argv[] )
{
  char *objName;
  char *usage;
  int exists;
  int unreg_status;
  RepositoryDataT obj_t;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  exists = gRepository->Exists( objName );
  if ( ! exists ) {
    Tcl_AppendResult( interp, "object ", objName, " not in repository",
		      (char *)NULL );
    return TCL_ERROR;
  }

  obj_t = gRepository->GetType( objName );
  if ( obj_t == SOLID_MODEL_T || obj_t == MESH_T || obj_t == ADAPTOR_T) {
    return Tcl_VarEval( interp, "rename ", objName, " {}", (char *)NULL );
  } else {
    unreg_status = gRepository->UnRegister( objName );
  }

  if ( ! unreg_status ) {
    Tcl_AppendResult( interp, "error deleting object ", objName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -------------
// Repos_TypeCmd
// -------------

int Repos_TypeCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char *name;
  RepositoryDataT type;
  char *typeStr;
  char *usage;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &name, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1, table_sz, arg_table )
       != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  if ( ! gRepository->Exists( name ) ) {
    Tcl_AppendResult( interp, "object ", name, " not found", (char*)NULL );
    return TCL_ERROR;
  }

  type = gRepository->GetType( name );
  typeStr = RepositoryDataT_EnumToStr( type );
  Tcl_SetResult( interp, typeStr, TCL_VOLATILE );
  delete [] typeStr;
  return TCL_OK;
}


// --------------------
// Repos_ImportVtkPdCmd
// --------------------
// The idea here is that we want to take an interpreter-level name for
// a vtk object, and use that to look up a pointer to that object and
// then to create a cvRepositoryData object from that pointer.

int Repos_ImportVtkPdCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *vtkName;
  char *objName;
  vtkPolyData *vtkObj;
  int status;
  cvPolyData *pd;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &vtkName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };    

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1, table_sz,
			arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Look up the named vtk object:
  vtkObj = (vtkPolyData *)vtkTclGetPointerFromObject( vtkName, "vtkPolyData",
						      interp, status );
  if ( vtkObj == NULL ) {
    Tcl_AppendResult( interp, "error retrieving vtk object ", vtkName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "obj ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  pd = new cvPolyData( vtkObj );
  pd->SetName( objName );
  if ( !( gRepository->Register( pd->GetName(), pd ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete pd;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, pd->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// --------------------
// Repos_ExportToVtkCmd
// --------------------

int Repos_ExportToVtkCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  RepositoryDataT type;
  cvRepositoryData *pd;
  vtkObject *vtkObj;

  int table_sz = 1;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };    

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1, table_sz,
			arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Retrieve source object:
  pd = gRepository->GetObject( objName );
  if ( pd == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
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
       ( type != UNSTRUCTURED_GRID_T ) && ( type != TEMPORALDATASET_T ) ) {

    Tcl_AppendResult( interp, objName, " not a data object", (char *)NULL );
    return TCL_ERROR;
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
  vtkTclGetObjectFromPointer( interp, (void *)vtkObj, NULL );

  // The newly-generated object name has already been put in the result.

  return TCL_OK;
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

int Repos_ImportVtkSpCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *vtkName;
  char *objName;
  vtkStructuredPoints *vtkObj;
  int status;
  cvStrPts *sp;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &vtkName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };    

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1, table_sz,
			arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Look up the named vtk object:
  vtkObj = (vtkStructuredPoints *)
    vtkTclGetPointerFromObject( vtkName, "vtkStructuredPoints",
				interp, status );
  if ( vtkObj == NULL ) {
    Tcl_AppendResult( interp, "error retrieving vtk object ", vtkName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "obj ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  sp = new cvStrPts( vtkObj );
  sp->SetName( objName );
  if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete sp;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, sp->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ----------------------------------
// Repos_ImportVtkUnstructuredGridCmd
// ----------------------------------

int Repos_ImportVtkUnstructuredGridCmd( ClientData clientData, Tcl_Interp *interp,
			  int argc, CONST84 char *argv[] )
{
  char *usage;
  char *vtkName;
  char *objName;
  vtkUnstructuredGrid *vtkObj;
  int status;
  cvUnstructuredGrid *sp;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &vtkName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };    

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1, table_sz,
			arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Look up the named vtk object:
  vtkObj = (vtkUnstructuredGrid *)
    vtkTclGetPointerFromObject( vtkName, "vtkUnstructuredGrid",
				interp, status );
  if ( vtkObj == NULL ) {
    Tcl_AppendResult( interp, "error retrieving vtk object ", vtkName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "obj ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

  sp = new cvUnstructuredGrid( vtkObj );
  sp->SetName( objName );
  if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete sp;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, sp->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// ---------------------
// Repos_ImportVtkImgCmd
// ---------------------

int Repos_ImportVtkImgCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  char *usage;
  char *vtkName;
  char *objName;
  vtkImageData *vtkObj;
  int status;
  cvStrPts *sp;

  int table_sz = 2;
  ARG_Entry arg_table[] = {
    { "-src", STRING_Type, &vtkName, NULL, REQUIRED, 0, { 0 } },
    { "-dst", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
  };    

  usage = ARG_GenSyntaxStr( 1, argv, table_sz, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1, table_sz,
			arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Look up the named vtk object:
  vtkObj = (vtkImageData *)
    vtkTclGetPointerFromObject( vtkName, "vtkImageData", interp, status );
  if ( vtkObj == NULL ) {
    Tcl_AppendResult( interp, "error retrieving vtk object ", vtkName,
		      (char *)NULL );
    return TCL_ERROR;
  }

  // Is the specified repository object name already in use?
  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "obj ", objName, " already exists",
		      (char *)NULL );
    return TCL_ERROR;
  }

/*
  vtkImageToStructuredPoints *i2sp;
  i2sp = vtkImageToStructuredPoints::New();
  i2sp->SetInputDataObject(vtkObj);
  i2sp->Update();
  sp = new cvStrPts( i2sp->GetOutput() );
  i2sp->Delete();
*/

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
  if ( !( gRepository->Register( sp->GetName(), sp ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    delete sp;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, sp->GetName(), TCL_VOLATILE );
  return TCL_OK;
}


// -------------
// Repos_SaveCmd
// -------------

int Repos_SaveCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char filename[2048];
  int saveResult;

  if ( argc != 2 ) {
    Tcl_SetResult( interp, "usage: repos_save <filename>", TCL_STATIC );
    return TCL_ERROR;
  }

  filename[0]='\0';
  sprintf(filename,"%s",argv[1]);
  
  saveResult = gRepository->Save( filename );
  if ( saveResult ) {
    Tcl_SetResult( interp, "repository successfully saved", TCL_STATIC );
    return TCL_OK;
  } else {
    Tcl_SetResult( interp, "error saving repository", TCL_STATIC );
    return TCL_ERROR;
  }
}


// -------------
// Repos_LoadCmd
// -------------

int Repos_LoadCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] )
{
  char filename[2048];
  int loadResult;

  if ( argc != 2 ) {
    Tcl_SetResult( interp, "usage: repos_load <filename>", TCL_STATIC );
    return TCL_ERROR;
  }

  filename[0]='\0';
  sprintf(filename,"%s",argv[1]);
  loadResult = gRepository->Load( filename );
  if ( loadResult ) {
    Tcl_SetResult( interp, "repository successfully loaded", TCL_STATIC );
    return TCL_OK;
  } else {
    Tcl_SetResult( interp, "error loading repository", TCL_STATIC );
    return TCL_ERROR;
  }

  return TCL_OK;
}


// -------------------------
// Repos_WriteVtkPolyDataCmd
// -------------------------

// repos_writeVtkPolyData -obj <objName> -type { bin | ascii } -file <outFn>

// I'm making this command part of the cvRepository package.  Note
// however that it only applies to objects in the cvRepository which are
// based on vtkPolyData.  See vtk/common/vtkSetGet.h for the macro
// used for methods like SetFileName.

int Repos_WriteVtkPolyDataCmd( ClientData clientData, Tcl_Interp *interp,
			       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName, *ft, *fn;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkPolyData *pd;

  // Define syntax:
  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-type", STRING_Type, &ft, NULL, REQUIRED, 0, { 0 } },
    { "-file", STRING_Type, &fn, NULL, REQUIRED, 0, { 0 } }
  };

  // Generate syntax string:
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // No add'l args given <--> print usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse, and return usage if error:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  type = gRepository->GetType( objName );
  if ( type != POLY_DATA_T ) {
    Tcl_AppendResult( interp, "\"", objName,
		      "\" must be of type cvPolyData", (char *)NULL );
    return TCL_ERROR;
  }

  obj = gRepository->GetObject( objName );
  switch (type) {
  case POLY_DATA_T:
    pd = ((cvPolyData *)obj)->GetVtkPolyData();
    break;
  default:
    Tcl_SetResult( interp, "error in GetVtkPolyData", TCL_STATIC );
    return TCL_ERROR;
    break;
  }

  vtkPolyDataWriter *pdWriter = vtkPolyDataWriter::New();
  pdWriter->SetInputDataObject( pd );
  pdWriter->SetFileName( fn );
  if ( strcmp( ft, "bin" ) == 0 ) {
    pdWriter->SetFileTypeToBinary();
  } else if ( strcmp( ft, "ascii" ) == 0 ) {
    pdWriter->SetFileTypeToASCII();
  }
  pdWriter->Write();

  pdWriter->Delete();

  return TCL_OK;
}


// ------------------------
// Repos_ReadVtkPolyDataCmd
// ------------------------

// repos_readVtkPolyData -obj <objName> -file <inFn>

int Repos_ReadVtkPolyDataCmd( ClientData clientData, Tcl_Interp *interp,
			      int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName, *fn;
  vtkPolyData *vtkPd;
  cvPolyData *pd;

  // Define syntax:
  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-file", STRING_Type, &fn, NULL, REQUIRED, 0, { 0 } }
  };

  // Generate syntax string:
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // No add'l args given <--> print usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse, and return usage if error:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  // Does file exist?
  if ( ( Tcl_VarEval( interp, "file exists ", fn, (char *) NULL ) != TCL_OK )
       || ( strcmp( Tcl_GetStringResult(interp), "0" ) == 0 ) ) {
    Tcl_ResetResult( interp );
    Tcl_AppendResult( interp, "error accessing file ", fn, (char *) NULL );
    return TCL_ERROR;
  }
  Tcl_ResetResult( interp );

  vtkPolyDataReader *pdReader = vtkPolyDataReader::New();
  pdReader->SetFileName( fn );

  // Note that it is critical to call Update even for vtk readers.
  pdReader->Update();

  vtkPd = pdReader->GetOutput();
  if ( vtkPd == NULL ) {
    Tcl_AppendResult( interp, "error reading file ", fn, (char *)NULL );
    pdReader->Delete();
    return TCL_ERROR;
  }

  if ( gRepository->Exists( objName ) ) {
    Tcl_AppendResult( interp, "obj ", objName, " already exists",
		      (char *)NULL );
    pdReader->Delete();
    return TCL_ERROR;
  }

  pd = new cvPolyData( vtkPd );
  if ( !( gRepository->Register( objName, pd ) ) ) {
    Tcl_AppendResult( interp, "error registering obj ", objName,
		      " in repository", (char *)NULL );
    pdReader->Delete();
    delete pd;
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, pd->GetName(), TCL_VOLATILE );
  pdReader->Delete();
  return TCL_OK;
}


// ---------------------------------
// Repos_WriteVtkStructuredPointsCmd
// ---------------------------------

// repos_writeVtkStructuredPoints -obj <str> -type <str> -file <str>

int Repos_WriteVtkStructuredPointsCmd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName, *ft, *fn;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkStructuredPoints *sp;

  // Define syntax:
  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-type", STRING_Type, &ft, NULL, REQUIRED, 0, { 0 } },
    { "-file", STRING_Type, &fn, NULL, REQUIRED, 0, { 0 } }
  };

  // Generate syntax string:
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // No add'l args given <--> print usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse, and return usage if error:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  type = gRepository->GetType( objName );
  if ( type != STRUCTURED_PTS_T ) {
    Tcl_AppendResult( interp, "\"", objName,
		      "\" must be of a structured points type", (char *)NULL );
    return TCL_ERROR;
  }

  obj = gRepository->GetObject( objName );
  switch (type) {
  case STRUCTURED_PTS_T:
    sp = ((cvStrPts *)obj)->GetVtkStructuredPoints();
    break;
  default:
    Tcl_SetResult( interp, "error in GetVtkStructuredPoints", TCL_STATIC );
    return TCL_ERROR;
    break;
  }

  vtkStructuredPointsWriter *spWriter = vtkStructuredPointsWriter::New();
  spWriter->SetInputDataObject( sp );
  spWriter->SetFileName( fn );
  if ( strcmp( ft, "bin" ) == 0 ) {
    spWriter->SetFileTypeToBinary();
  } else if ( strcmp( ft, "ascii" ) == 0 ) {
    spWriter->SetFileTypeToASCII();
  }
  spWriter->Write();

  spWriter->Delete();

  return TCL_OK;
}


// ---------------------------------
// Repos_WriteVtkUnstructuredGridCmd
// ---------------------------------

// repos_writeVtkUnstructuredGrid -obj <str> -type <str> -file <str>

int Repos_WriteVtkUnstructuredGridCmd( ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName, *ft, *fn;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkUnstructuredGrid *sp;

  // Define syntax:
  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-type", STRING_Type, &ft, NULL, REQUIRED, 0, { 0 } },
    { "-file", STRING_Type, &fn, NULL, REQUIRED, 0, { 0 } }
  };

  // Generate syntax string:
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

  // No add'l args given <--> print usage:
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }

  // Parse, and return usage if error:
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  type = gRepository->GetType( objName );
  if ( type != UNSTRUCTURED_GRID_T ) {
    Tcl_AppendResult( interp, "\"", objName,
		      "\" must be of a structured points type", (char *)NULL );
    return TCL_ERROR;
  }

  obj = gRepository->GetObject( objName );
  switch (type) {
  case UNSTRUCTURED_GRID_T:
    sp = ((cvUnstructuredGrid*)obj)->GetVtkUnstructuredGrid();
    break;
  default:
    Tcl_SetResult( interp, "error in GetVtkUnstructuredGrid", TCL_STATIC );
    return TCL_ERROR;
    break;
  }

  vtkUnstructuredGridWriter *spWriter = vtkUnstructuredGridWriter::New();
  spWriter->SetInputDataObject( sp );
  spWriter->SetFileName( fn );
  if ( strcmp( ft, "bin" ) == 0 ) {
    spWriter->SetFileTypeToBinary();
  } else if ( strcmp( ft, "ascii" ) == 0 ) {
    spWriter->SetFileTypeToASCII();
  }
  spWriter->Write();

  spWriter->Delete();

  return TCL_OK;
}


// ---------------------
// Repos_GetLabelKeysCmd
// ---------------------

int Repos_GetLabelKeysCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  int numKeys, i;
  char **keys;

  int table_size = 1;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  obj->GetLabelKeys( &numKeys, &keys );
  for (i = 0; i < numKeys; i++) {
    Tcl_AppendElement( interp, keys[i] );
  }

  return TCL_OK;
}


// -----------------
// Repos_GetLabelCmd
// -----------------

int Repos_GetLabelCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  char *key, *value;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-key", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  if ( ! obj->GetLabel( key, &value ) ) {
    Tcl_AppendResult( interp, "key ", key, " not found", (char *) NULL );
    return TCL_ERROR;
  }

  Tcl_SetResult( interp, value, TCL_VOLATILE );
  return TCL_OK;
}


// -----------------
// Repos_SetLabelCmd
// -----------------

int Repos_SetLabelCmd( ClientData clientData, Tcl_Interp *interp,
		       int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  char *key, *value;

  int table_size = 3;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-key", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } },
    { "-value", STRING_Type, &value, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  if ( ! obj->SetLabel( key, value ) ) {
    if ( obj->IsLabelPresent( key ) ) {
      Tcl_AppendResult( interp, "key ", key, " already in use",	(char *)NULL );
      return TCL_ERROR;
    } else {
      Tcl_AppendResult( interp, "error setting label ", key, (char *)NULL );
      return TCL_ERROR;
    }
  }

  return TCL_OK;
}


// -------------------
// Repos_ClearLabelCmd
// -------------------

int Repos_ClearLabelCmd( ClientData clientData, Tcl_Interp *interp,
			 int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  cvRepositoryData *obj;
  char *key;

  int table_size = 2;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-key", STRING_Type, &key, NULL, REQUIRED, 0, { 0 } }
  };
  usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );
  if ( argc == 1 ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_OK;
  }
  if ( ARG_ParseTclStr( interp, argc, argv, 1,
			table_size, arg_table ) != TCL_OK ) {
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:

  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    Tcl_AppendResult( interp, "couldn't find object ", objName, (char *)NULL );
    return TCL_ERROR;
  }

  if ( ! obj->IsLabelPresent( key ) ) {
    Tcl_AppendResult( interp, "key ", key, " not found", (char *) NULL );
    return TCL_ERROR;
  }
    
  obj->ClearLabel( key );

  return TCL_OK;
}
