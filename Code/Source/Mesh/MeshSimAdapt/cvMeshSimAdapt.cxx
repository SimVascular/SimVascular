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

/** @file cvMeshSimAdapt.cxx
 *  @brief The implementations of functions in cvMeshSimAdapt
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "cvMeshSimAdapt.h"

#include "cv_adapt_utils.h"

#include "vtkXMLUnstructuredGridReader.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkXMLPolyDataReader.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkGradientFilter.h"
#include "vtkDoubleArray.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"

#include "cvRepository.h"
#include "cvRepositoryData.h"
#include "cv_globals.h"

#include <iostream>

cvMeshSimAdapt::cvMeshSimAdapt() 
  : cvAdaptObject(KERNEL_MESHSIM)
{
  meshobject_ = NULL;
  inmesh_  = NULL;
  insurface_mesh_ = NULL;

  options.poly_ = 1;
  options.strategy_ = 1;
  options.ratio_ = 0.2;
  options.nvar_ = 5;
  options.hmax_ = 1;
  options.hmin_ = 1;
  options.instep_ = 0;
  options.outstep_ = 0;
  options.ndof_=5;

  sol_array_ = NULL;
  hessians_array_ = NULL;
  ybar_array_ = NULL;

  sol_ = NULL;
  hessians_ = NULL;
  ybar_ = NULL;
}

cvMeshSimAdapt::cvMeshSimAdapt( const cvMeshSimAdapt& Adapt)
  : cvAdaptObject( KERNEL_MESHSIM)
{
  Copy(Adapt);
}


cvMeshSimAdapt::~cvMeshSimAdapt()
{
  if (inmesh_ != NULL)
    inmesh_->Delete();
  if (insurface_mesh_ != NULL)
    insurface_mesh_->Delete();

  if (sol_array_ != NULL)
    sol_array_->Delete();
  if (hessians_array_ != NULL)
    hessians_array_->Delete();
  if (ybar_array_ != NULL)
    ybar_array_->Delete();

  if (sol_ != NULL)
    delete [] sol_;
  if (ybar_ != NULL)
    delete [] ybar_;
  if (hessians_ != NULL)
    delete [] hessians_;

  if (meshobject_ != NULL)
    gRepository->UnRegister(meshobject_->GetName());
}

cvAdaptObject *cvMeshSimAdapt::Copy() const
{
  cvMeshSimAdapt *result = new cvMeshSimAdapt( *this );
  return result;
}

int cvMeshSimAdapt::Copy( const cvAdaptObject& src)
{
  cvMeshSimAdapt *adaptPtr;

  adaptPtr = (cvMeshSimAdapt *)( &src );

  return CV_OK;
}

// -----------------------
//  CreateInternalMeshObject
// -----------------------
int cvMeshSimAdapt::CreateInternalMeshObject(Tcl_Interp *interp)
{
  char* mesh_name = NULL;
  if (meshobject_ != NULL)
  {
    fprintf(stderr,"Cannot create a mesh object, one already exists\n");
    return CV_ERROR;
  }

  cvMeshObject::KernelType newkernel = cvMeshObject::GetKernelType("MeshSim");
  meshobject_ = cvMeshSystem::DefaultInstantiateMeshObject( interp,"dummy","dummy");
  if ( meshobject_ == NULL ) {
    fprintf(stderr,"Mesh Object is null after instantiation!\n");
    return CV_ERROR;
  }

  mesh_name = "/adapt/internal/meshobject";

  // TODO: Previously had this, but was causing crashing after a few runs.
  // Didn't check and unregister if didn't work, crashing gone. This 
  // doesn't seem good. Need to figure out what is really happening.
  // Register the solid:
  //if ( !( gRepository->Register(mesh_name, meshobject_ ) ) ) {
  //  Tcl_AppendResult( interp, "error registering obj ", mesh_name,
  //      	      " in repository", (char *)NULL );
  //  fprintf(stderr,"Error when registering\n");
  //  int unreg_status = gRepository->UnRegister( mesh_name );
  //  return CV_ERROR;
  //}
  gRepository->Register(mesh_name, meshobject_ );

 return CV_OK;
}


// -----------------------
//  LoadModel
// -----------------------
int cvMeshSimAdapt::LoadModel(char *fileName)
{
  if (meshobject_ == NULL)
  {
    fprintf(stderr,"Must create internal mesh object with CreateInternalMeshObject()\n");
    return CV_ERROR;
  }

  if (meshobject_->LoadModel(fileName) != CV_OK)
  {
    fprintf(stderr,"Error loading solid model\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// -----------------------
//  LoadMesh
// -----------------------
int cvMeshSimAdapt::LoadMesh(char *fileName)
{
  const char *extension = strrchr(fileName,".");
  extension = extension+1;

  //if loading vtu, save as member data
  //else if loading sms, load into meshobject
  if (!strncmp(extension,"vtu",3)) {
    if (inmesh_ != NULL)
      inmesh_->Delete();
    vtkSmartPointer<vtkXMLUnstructuredGridReader> ugreader = 
      vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();
    inmesh_ = vtkUnstructuredGrid::New();
    ugreader->SetFileName(fileName);
    ugreader->Update();
    inmesh_->DeepCopy(ugreader->GetOutput());
    printf("\n-- Loaded Mesh...\n");
    printf(" Total # of elements: %d\n", inmesh_->GetNumberOfCells());
    printf(" Total # of vertices: %d\n", inmesh_->GetNumberOfPoints());
    inmesh_->BuildLinks();
  } else if (!strncmp(extension,"sms",3)) {
    if (meshobject_ == NULL)
    {
      fprintf(stderr,"Must create internal mesh object with CreateInternalMeshObject()\n");
      return CV_ERROR;
    }
    char *dummy = NULL;
    meshobject_->LoadMesh(fileName,dummy);
  }

  return CV_OK;
}

// ---------------
//  LoadSolution
// ---------------
int cvMeshSimAdapt::LoadSolutionFromFile(char *fileName)
{
  if (sol_ != NULL)
    delete [] sol_;

  AdaptUtils_readArrayFromFile(fileName,"solution",sol_);

  if (AdaptUtils_attachArray(sol_,inmesh_,"solution",options.ndof_,options.poly_) != CV_OK)
  {
    fprintf(stderr,"Error: Error when attaching solution to mesh\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// ---------------
//  LoadYbar
// ---------------
int cvMeshSimAdapt::LoadYbarFromFile(char *fileName)
{
  if (ybar_ != NULL)
    delete [] ybar_;

  AdaptUtils_readArrayFromFile(fileName,"ybar",ybar_);

  if (AdaptUtils_attachArray(ybar_,inmesh_,"error",options.nvar_,options.poly_) != CV_OK)
  {
    fprintf(stderr,"Error: Error when attaching error to mesh\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// ---------------
//  LoadHessian
// ---------------
int cvMeshSimAdapt::LoadHessianFromFile(char *fileName)
{
  if (hessians_ != NULL)
    delete [] hessians_;

  AdaptUtils_readArrayFromFile(fileName,"hessians",hessians_);

  return CV_OK;
}

// ---------------
//  ReadSolution
// ---------------
int cvMeshSimAdapt::ReadSolutionFromMesh()
{
  if (inmesh_ == NULL)
  {
    fprintf(stderr,"Must load mesh before checking to see if solution exists\n");
    return CV_ERROR;
  }

  if (AdaptUtils_checkArrayExists(inmesh_,0,"solution") != CV_OK)
    return CV_ERROR;

  if (sol_array_ != NULL)
    sol_array_->Delete();

  int numPoints = inmesh_->GetNumberOfPoints();
  sol_array_ = vtkDoubleArray::New();
  sol_array_->SetNumberOfComponents(options.nvar_);

  return CV_OK;
}

// ---------------
//  ReadYbar
// ---------------
int cvMeshSimAdapt::ReadYbarFromMesh()
{
  if (inmesh_ == NULL)
  {
    fprintf(stderr,"Must load mesh before checking to see if solution exists\n");
    return CV_ERROR;
  }

  if (AdaptUtils_checkArrayExists(inmesh_,0,"ybar") != CV_OK)
    return CV_ERROR;

  return CV_OK;
}

// -----------------------
//  SetAdaptOptions
// -----------------------
/** 
 * @brief Function to set the options for tetgen. Store temporarily in 
 * options object until the mesh is run
 * @param *flag char containing the flag to set
 * @param value if the flag requires a value, this double contains that 
 * value to be set
 * @return *result: CV_ERROR if the flag doesn't exist. Else return CV_OK
 */
int cvMeshSimAdapt::SetAdaptOptions(char *flag,double value)
{
  fprintf(stderr,"At beginning of options\n");
  if (!strncmp(flag,"poly",4)) {
    options.poly_ = (int) value;
  }
  else if (!strncmp(flag,"instep",6)) {
    options.instep_ = (int) value;
  }
  else if (!strncmp(flag,"outstep",6)) {
    options.outstep_ = (int) value;
  }
  else if (!strncmp(flag,"strategy",8)) {
    options.strategy_ = (int) value;
  }
  else if (!strncmp(flag,"ratio",5)) {
    options.ratio_ = (double) value;
  }
  else if (!strncmp(flag,"nvar",4)) {
    options.nvar_ = (int) value;
  }
  else if (!strncmp(flag,"hmax",4)) {
    options.hmax_ = (double) value;
  }
  else if (!strncmp(flag,"hmin",4)) {
    options.hmin_ = (double) value;
  }
  else if (!strncmp(flag,"ndof",8)) {
    options.ndof_ = (int) value;
  }
  else {
    fprintf(stderr,"Flag given is not a valid adapt option\n");
    return CV_ERROR;
  }

  fprintf(stderr,"At end of options\n");
  return CV_OK;
}

// -----------------------
//  CheckOptions
// -----------------------
int cvMeshSimAdapt::CheckOptions() 
{
  fprintf(stderr,"Check values\n");
  fprintf(stderr,"Poly: %d\n",options.poly_);
  fprintf(stderr,"Strategy: %d\n",options.strategy_);
  fprintf(stderr,"Ratio: %.4f\n",options.ratio_);
  fprintf(stderr,"NVar: %d\n",options.nvar_);
  fprintf(stderr,"Hmax: %.4f\n",options.hmax_);
  fprintf(stderr,"Hmin: %.4f\n",options.hmin_);
  fprintf(stderr,"Ndof: %d\n",options.ndof_);

  return CV_ERROR;
}

// -----------------------
//  SetErrorMetric
// -----------------------
int cvMeshSimAdapt::SetErrorMetric()
{
  fprintf(stdout,"TODO\n");
  return CV_OK;
}

// -----------------------
//  SetUpMesh
// -----------------------
int cvMeshSimAdapt::SetupMesh()
{
  fprintf(stdout,"TODO\n");
  return CV_OK;
}

// -----------------------
//  RunAdaptor
// -----------------------
int cvMeshSimAdapt::RunAdaptor()
{
  if (meshobject_ == NULL)
  {
    fprintf(stderr,"Must create internal mesh object with CreateInternalMeshObject()\n");
    return CV_ERROR;
  }
  meshobject_->Adapt();
  meshobject_->SetError(ybar_,options.instep_,options.ratio_,options.hmax_,options.hmin_);
  return CV_OK;
}

// -----------------------
//  PrintStats
// -----------------------
int cvMeshSimAdapt::PrintStats()
{
  fprintf(stdout,"TODO\n");
  return CV_OK;
}

// -----------------------
//  GetAdaptedMesh
// -----------------------
int cvMeshSimAdapt::GetAdaptedMesh()
{
  fprintf(stdout,"TODO!\n");
  return CV_OK;
}

// -----------------------
//  TransferSolution
// -----------------------
int cvMeshSimAdapt::TransferSolution()
{
  //if (AdaptUtils_fix4SolutionTransfer(inmesh_,outmesh_,options.ndof_) != CV_OK)
  //{
  //  fprintf(stderr,"ERROR: Solution was not transferred\n");
  //  return CV_ERROR;
  //}

  fprintf(stdout,"TODO!\n");
  return CV_OK;
}


// -----------------------
//  WriteAdaptedModel
// -----------------------
int cvMeshSimAdapt::WriteAdaptedModel(char *fileName)
{
  fprintf(stdout,"TODO!\n");
  return CV_OK;
}

// -----------------------
//  WriteAdaptedMesh
// -----------------------
int cvMeshSimAdapt::WriteAdaptedMesh(char *fileName)
{
  fprintf(stdout,"TODO!\n");
  return CV_OK;
}

// -----------------------
//  WriteAdaptedSolution
// -----------------------
int cvMeshSimAdapt::WriteAdaptedSolution(char *fileName)
{
  fprintf(stdout,"TODO!\n");
  //int numPoints = outmesh_->GetNumberOfPoints();
  //AdaptUtils_writeArrayToFile(fileName,"solution","binary","write",numPoints,
  //    options.ndof_,options.outstep_,solution);

  //delete [] solution;
  return CV_OK;
}

