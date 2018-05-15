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
#include "sv_Repository.h"
#include "sv_solid_init.h"
#include "sv_occt_init.h"
#include "sv_SolidModel.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"
#include "sv_sys_geom.h"
#include "sv_OCCTSolidModel.h"

#include "sv_FactoryRegistrar.h"

#include "vtkSmartPointer.h"
#include "vtkSVNURBSSurface.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

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

int OCCTSolidModel_loftVtksvNURBSCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] );

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
  Tcl_CreateCommand( interp, "occt_loft_vtksv_nurbs", OCCTSolidModel_loftVtksvNURBSCmd,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );

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

// ---------------------
// OCCTSolidModel_loftVtksvNURBSCmd
// ---------------------
int OCCTSolidModel_loftVtksvNURBSCmd( ClientData clientData, Tcl_Interp *interp,
			   int argc, CONST84 char *argv[] )
{
  char *usage;
  char *objName;
  int numSrcs;
  ARG_List srcList;
  cvRepositoryData *src;
  cvPolyData *loftedPd;
  RepositoryDataT type;
  cvPolyData **srcs;
  int uDegree = 2;
  int vDegree = 2;
  char *uKnotSpanType;
  char *vKnotSpanType;
  char *uParametricSpanType;
  char *vParametricSpanType;

  int table_size = 8;
  ARG_Entry arg_table[] = {
    { "-obj", STRING_Type, &objName, NULL, REQUIRED, 0, { 0 } },
    { "-srclist", LIST_Type, &srcList, NULL, REQUIRED, 0, { 0 } },
    { "-uDegree", INT_Type, &uDegree, NULL, REQUIRED, 0, { 0 } },
    { "-vDegree", INT_Type, &vDegree, NULL, REQUIRED, 0, { 0 } },
    { "-uKnotSpanType", STRING_Type, &uKnotSpanType, NULL, REQUIRED, 0, { 0 } },
    { "-vKnotSpanType", STRING_Type, &vKnotSpanType, NULL, REQUIRED, 0, { 0 } },
    { "-uParametricSpanType", STRING_Type, &uParametricSpanType, NULL, REQUIRED, 0, { 0 } },
    { "-vParametricSpanType", STRING_Type, &vParametricSpanType, NULL, REQUIRED, 0, { 0 } },
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

  //Call cvOCCTSolidModel function to create BSpline surf
  cvOCCTSolidModel *geom;
  if (cvSolidModel::gCurrentKernel != SM_KT_OCCT)
  {
    fprintf(stderr,"Solid Model kernel must be OCCT\n");
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  geom = (cvOCCTSolidModel *)gRepository->GetObject( objName );
  if ( geom == NULL ) {
    fprintf(stderr,"Object is not in repository\n");
    Tcl_SetResult( interp, usage, TCL_VOLATILE );
    return TCL_ERROR;
  }

  // Do work of command:
  numSrcs = srcList.argc;

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvSolidModel).  Also build up the array of
  // cvSolidModel*'s to pass to cvSolidModel::MakeLoftedSurf.

  srcs = new cvPolyData * [numSrcs];

  for (int i = 0; i < numSrcs; i++ ) {
    src = gRepository->GetObject( srcList.argv[i] );
    if ( src == NULL ) {
      Tcl_AppendResult( interp, "couldn't find object ", srcList.argv[i],
			(char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] srcs;
      return TCL_ERROR;
    }
    type = src->GetType();
    if ( type != POLY_DATA_T ) {
      Tcl_AppendResult( interp, "object ", srcList.argv[i],
			" not of type cvPolyData", (char *)NULL );
      ARG_FreeListArgvs( table_size, arg_table );
      delete [] srcs;
      return TCL_ERROR;
    }
    srcs[i] = (cvPolyData *) src;
  }

  // We're done with the src object names:
  ARG_FreeListArgvs( table_size, arg_table );

  double dummySpacing = 0.5;
  vtkSmartPointer<vtkSVNURBSSurface> NURBSSurface =
    vtkSmartPointer<vtkSVNURBSSurface>::New();
  if ( sys_geom_loft_solid_with_nurbs(srcs, numSrcs, uDegree, vDegree, dummySpacing,
                                      dummySpacing, uKnotSpanType, vKnotSpanType,
                                      uParametricSpanType, vParametricSpanType,
                                      NURBSSurface,
			                                (cvPolyData**)(&loftedPd) ) != SV_OK ) {
    Tcl_SetResult( interp, "poly manipulation error", TCL_STATIC );
    delete loftedPd;
    delete [] srcs;
    return TCL_ERROR;
  }

  delete loftedPd;

  // Now convert all NURBS info to arrays for creation of bspline
  // Control points
  int dims[3];
  NURBSSurface->GetControlPointGrid()->GetDimensions(dims);
  int Xlen1 = dims[0]; int Xlen2 = dims[1];
  double **Xarr = new double*[Xlen1];
  double **Yarr = new double*[Xlen1];
  double **Zarr = new double*[Xlen1];
  for (int i=0; i<Xlen1; i++)
  {
    Xarr[i] = new double[Xlen2];
    Yarr[i] = new double[Xlen2];
    Zarr[i] = new double[Xlen2];
  }

  int ptId;
  double pt[3];
  for (int i=0; i<Xlen1; i++)
  {
    for (int j=0; j<Xlen2; j++)
    {
      NURBSSurface->GetControlPointGrid()->GetPointId(i, j, 0, ptId);
      NURBSSurface->GetControlPointGrid()->GetPoint(ptId, pt);
      Xarr[i][j] = pt[0];
      Yarr[i][j] = pt[1];
      Zarr[i][j] = pt[2];
    }
  }

  // Knots and multiplicities
  vtkSmartPointer<vtkDoubleArray> singleUKnots =
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkDoubleArray> singleVKnots =
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkIntArray> uMult =
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> vMult =
    vtkSmartPointer<vtkIntArray>::New();

  NURBSSurface->GetUMultiplicity(uMult, singleUKnots);
  NURBSSurface->GetVMultiplicity(vMult, singleVKnots);

  int uKlen = singleUKnots->GetNumberOfTuples();
  double *uKarr = new double[uKlen];
  for (int i=0; i<uKlen; i++)
    uKarr[i] = singleUKnots->GetTuple1(i);

  int vKlen = singleVKnots->GetNumberOfTuples();
  double *vKarr = new double[vKlen];
  for (int i=0; i<vKlen; i++)
    vKarr[i] = singleVKnots->GetTuple1(i);

  int uMlen = uMult->GetNumberOfTuples();
  double *uMarr = new double[uMlen];
  for (int i=0; i<uMlen; i++)
    uMarr[i] = uMult->GetTuple1(i);

  int vMlen = vMult->GetNumberOfTuples();
  double *vMarr = new double[vMlen];
  for (int i=0; i<vMlen; i++)
    vMarr[i] = vMult->GetTuple1(i);

  //int p = NURBSSurface->GetUDegree();  vtksv needs update
  //int q = NURBSSurface->GetVDegree();
  int p = uDegree;
  int q = vDegree;

      // Flipping order!
  if (geom->CreateBSplineSurface(Xarr,Yarr,Zarr,Xlen1,Xlen2,
    vKarr,vKlen,uKarr,uKlen,vMarr,vMlen,uMarr,uMlen,q,p) != SV_OK)
  {
    Tcl_AppendResult( interp, "error lofting obj ", objName,
		      " in repository", (char *)NULL );
    delete [] uKarr;
    delete [] vKarr;
    delete [] uMarr;
    delete [] vMarr;

    for (int i=0; i<Xlen1; i++)
    {
      delete [] Xarr[i];
      delete [] Yarr[i];
      delete [] Zarr[i];
    }
    delete [] Xarr;
    delete [] Yarr;
    delete [] Zarr;

    delete [] srcs;
    return TCL_ERROR;
  }

  delete [] uKarr;
  delete [] vKarr;
  delete [] uMarr;
  delete [] vMarr;

  for (int i=0; i<Xlen1; i++)
  {
    delete [] Xarr[i];
    delete [] Yarr[i];
    delete [] Zarr[i];
  }
  delete [] Xarr;
  delete [] Yarr;
  delete [] Zarr;

  delete [] srcs;

  char qstring[2048];
  qstring[0]='\0';
  sprintf(qstring,"%lf",q);
  Tcl_SetResult( interp, qstring, TCL_VOLATILE );
  return TCL_OK;
}
