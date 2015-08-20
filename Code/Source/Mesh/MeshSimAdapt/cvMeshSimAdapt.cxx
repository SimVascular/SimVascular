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
#include "cv_meshsim_adapt_utils.h"

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
#include "cvUnstructuredGrid.h"
#include "cvPolyData.h"
#include "cvVTK.h"
#include "cv_globals.h"

#include <iostream>

cvMeshSimAdapt::cvMeshSimAdapt() 
  : cvAdaptObject(KERNEL_MESHSIM)
{
  meshobject_ = NULL;
  inmesh_  = NULL;
  outmesh_  = NULL;
  insurface_mesh_ = NULL;
  outsurface_mesh_ = NULL;

  options.poly_ = 1;
  options.strategy_ = 1;
  options.ratio_ = 0.2;
  options.nvar_ = 5;
  options.hmax_ = 1;
  options.hmin_ = 1;
  options.instep_ = 0;
  options.outstep_ = 0;
  options.ndof_=5;
  options.sphere_[0] = -1;
  options.sphere_[1] = 0;
  options.sphere_[2] = 0;
  options.sphere_[3] = 0;
  options.sphere_[4] = 1;
  options.tmp_old_stuffs_ = 0;

  sol_array_ = NULL;
  hessians_array_ = NULL;
  ybar_array_ = NULL;

  sol_ = NULL;
  hessians_ = NULL;
  ybar_ = NULL;
  errormetric_ = NULL;
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
  if (errormetric_ != NULL)
    delete [] errormetric_;

  //if (meshobject_ != NULL)
  //{
  //  fprintf(stderr,"Mesh object not null, unregistering\n");
  //  gRepository->UnRegister(meshobject_->GetName());
  //}
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

  //// TODO: Previously had this, but was causing crashing after a few runs.
  //// Didn't check and unregister if didn't work, crashing gone. This 
  //// doesn't seem good. Need to figure out what is really happening.
  //// Register the solid:
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
  const char *extension = strrchr(fileName,'.');
  extension = extension +1;

  //if loading vtu, save as member data
  //else if loading sms, load into meshobject
  fprintf(stdout,"Reading Mesh!\n");

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
  } 
  else if (!strncmp(extension,"sms",3)) {
    fprintf(stdout,"SMS!\n");
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

  if (inmesh_ != NULL)
  {
    if (AdaptUtils_attachArray(sol_,inmesh_,"solution",options.ndof_,options.poly_) != CV_OK)
    {
      fprintf(stderr,"Error: Error when attaching solution to mesh\n");
      return CV_ERROR;
    }
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

  if (inmesh_ != NULL)
  {
    if (AdaptUtils_attachArray(ybar_,inmesh_,"error",options.nvar_,options.poly_) != CV_OK)
    {
      fprintf(stderr,"Error: Error when attaching error to mesh\n");
      return CV_ERROR;
    }
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

  if (inmesh_ != NULL)
  {
    if (AdaptUtils_attachArray(hessians_,inmesh_,"hessians",options.nvar_,options.poly_) != CV_OK)
    {
      fprintf(stderr,"Error: Error when attaching error to mesh\n");
      return CV_ERROR;
    }
  }

  return CV_OK;
}

// ---------------
//  ReadSolution
// ---------------
int cvMeshSimAdapt::ReadSolutionFromMesh()
{
 // if (inmesh_ == NULL)
 // {
 //   fprintf(stderr,"Must load mesh before checking to see if solution exists\n");
 //   return CV_ERROR;
 // }

 // if (AdaptUtils_checkArrayExists(inmesh_,0,"solution") != CV_OK)
 //   return CV_ERROR;

 // if (sol_array_ != NULL)
 //   sol_array_->Delete();

 // int numPoints = inmesh_->GetNumberOfPoints();
 // sol_array_ = vtkDoubleArray::New();
 // sol_array_->SetNumberOfComponents(options.nvar_);

  return CV_OK;
}

// ---------------
//  ReadYbar
// ---------------
int cvMeshSimAdapt::ReadYbarFromMesh()
{
  //if (inmesh_ == NULL)
  //{
  //  fprintf(stderr,"Must load mesh before checking to see if solution exists\n");
  //  return CV_ERROR;
  //}

  //if (AdaptUtils_checkArrayExists(inmesh_,0,"ybar") != CV_OK)
  //  return CV_ERROR;

  return CV_OK;
}

// -----------------------
//  SetAdaptOptions
// -----------------------
/** 
 * @brief Function to set the options for meshsim. Store temporarily in 
 * options object until the mesh is run
 * @param *flag char containing the flag to set
 * @param value if the flag requires a value, this double contains that 
 * value to be set
 * @return *result: CV_ERROR if the flag doesn't exist. Else return CV_OK
 */
int cvMeshSimAdapt::SetAdaptOptions(char *flag,double value)
{
  if (!strncmp(flag,"poly",4)) {
    options.poly_ = (int) value;
  }
  else if (!strncmp(flag,"instep",6)) {
    options.instep_ = (int) value;
  }
  else if (!strncmp(flag,"outstep",7)) {
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
  else if (!strncmp(flag,"ndof",4)) {
    options.ndof_ = (int) value;
  }
  else if (!strncmp(flag,"old",3)) {
    options.tmp_old_stuffs_ = (int) value;
  }
  else {
    fprintf(stderr,"Flag given is not a valid adapt option\n");
    return CV_ERROR;
  }

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

  return CV_OK;
}

// -----------------------
//  SetErrorMetric
// -----------------------
int cvMeshSimAdapt::SetErrorMetric()
{
  if (options.tmp_old_stuffs_ == 1)
  {
    //OLD STUFFS!
    if (meshobject_ == NULL)
    {
      fprintf(stderr,"Must create internal mesh object with CreateInternalMeshObject()\n");
      return CV_ERROR;
    }
                                                      
    int nvar = 5;
    char *arrayName = "error indicator";
    meshobject_->SetArrayOnMesh(ybar_,5,arrayName);
  }
  else 
  {
    if (inmesh_ == NULL)
    {
      fprintf(stderr,"Error: Mesh must be loaded to set hessians\n");
      return CV_ERROR;
    }
    int numPoints = inmesh_->GetNumberOfPoints();

    switch(options.strategy_) {
    //this code processes for both if strategy == 1 || strategy ==2
    //Right now the only implemented adaptation is for isotropic meshing. 
    //TetGen only has the ability to specify one size metric at each node 
    //within the mesh, so anisotropic meshing is not capable at this moment.
    //Strategies 1 and 2 implement isotropic adaptation 
    case 1 :
    case 2 : { //isotropic adaptation
      cout<<"\nStrategy chosen for ANISOTROPIC adaptation : size-field driven"<<endl;
      
      char error_tag[28];
      if(options.strategy_ == 1) {
        cout<<"\nUsing ybar to compute hessians...\n"<<endl;
        sprintf(error_tag,"ybar");
      }
      else if (options.strategy_ == 2) {
        cout<<"\nUsing numerical/computed hessians (i.e, from phasta)...\n"<<endl;
        sprintf(error_tag,"hessains");
      }

      if(options.strategy_==1) {
        // calculating hessians for ybar field
        // first reconstruct gradients and then the hessians 
        // also deals with boundary issues &
        // applies smoothing procedure for hessians
        // (simple average : arithmetic mean)
        if (AdaptUtils_hessiansFromSolution(inmesh_) != CV_OK)
        {
          fprintf(stderr,"Error: Error when calculating hessians from solution\n");
          return CV_ERROR;
        }
      }
      else if (options.strategy_ == 2) { // cannot use analytic hessian in this case
        // use the hessians computed from phasta
      }
      if (AdaptUtils_setSizeFieldUsingHessians(inmesh_,options.ratio_,options.hmax_,options.hmin_,options.sphere_,2) != CV_OK)
      {
          fprintf(stderr,"Error: Error when setting size field with hessians\n");
          return CV_ERROR;
      }
    }
    break;
    case 3:
    case 4: { // anisotropic adaptation (tag driven)
      cout<<"Strategy has not been implemented"<<endl;
      return 0;
    }
    break;
    case 5:
    case 6: { //anisotropic adaptation (size-field driven)
      cout<<"Strategy has not been implemented"<<endl;
      return 0;
    }
    break;
    default : {
      if(options.strategy_<0) {
        cout<<"This is default case but has not been implemented"<<endl;

      }
      else {
        cout<<"\nSpecify a correct (adaptation) strategy (adapt.cc)"<<endl;
        exit(-1);
      }
    }
    break;
    }

  }
  return CV_OK;
}

// -----------------------
//  SetUpMesh
// -----------------------
int cvMeshSimAdapt::SetupMesh()
{
  if (meshobject_ == NULL)
  {
    fprintf(stderr,"Mesh object needs to be create with CreateInternalMeshObject\n");
    return CV_ERROR;
  }

  if (inmesh_ == NULL)
  {
    fprintf(stderr,"ERROR: Volumetric vtu mesh is not loaded\n");
    return CV_ERROR;
  }

  if (AdaptUtils_checkArrayExists(inmesh_,0,"errormetric") != CV_OK)
  {
    fprintf(stderr,"Error metric must be incident on mesh. Created in SetErrorMetric\n");
    return CV_ERROR;
  }

  if (errormetric_ != NULL)
    delete [] errormetric_;

  int nVar = 9;
  int poly=1;
  if (AdaptUtils_getAttachedArray(errormetric_,inmesh_,"errormetric",nVar,poly) != CV_OK)
  {
    fprintf(stderr,"Error in getting error metric off mesh\n");
    return CV_ERROR;
  }

  meshobject_->SetErrorMetric(errormetric_,options.instep_,options.ratio_,options.hmax_,options.hmin_,options.tmp_old_stuffs_);


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
  if (ybar_ == NULL)
  {
    fprintf(stderr,"Must load ybar before running adaptor\n");
    return CV_ERROR;
  }
  //OLD STUFFS
  if (options.tmp_old_stuffs_ == 1)
  {
    meshobject_->SetErrorMetric(ybar_,options.instep_,options.ratio_,options.hmax_,options.hmin_,options.tmp_old_stuffs_);
  }

  meshobject_->Adapt();
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
  if (outmesh_ != NULL)
    outmesh_->Delete();

  if (outsurface_mesh_ != NULL)
    outsurface_mesh_->Delete();

  if (meshobject_ == NULL)
  {
    fprintf(stderr,"Mesh Object is null!\n");
    return CV_ERROR;
  }
  outmesh_ = vtkUnstructuredGrid::New();
  outsurface_mesh_ = vtkPolyData::New();
  meshobject_->GetAdaptedMesh(outmesh_,outsurface_mesh_,options.nvar_);

  //cvUnstructuredGrid *ug = meshobject_->GetUnstructuredGrid();
  //if  (ug == NULL)
  //{
  //  fprintf(stderr,"Could not get unstructured grid!\n");
  //  return CV_ERROR;
  //}
  //outmesh_->DeepCopy(ug->GetVtkUnstructuredGrid());

  //if (outsurface_mesh_ != NULL)
  //  outsurface_mesh_->Delete();

  //cvPolyData *pd = meshobject_->GetPolyData();
  //if  (pd == NULL)
  //{
  //  fprintf(stderr,"Could not get polydata!\n");
  //  return CV_ERROR;
  //}
  //outsurface_mesh_->DeepCopy(pd->GetVtkPolyData());

  return CV_OK;
}

// -----------------------
//  TransferSolution
// -----------------------
int cvMeshSimAdapt::TransferSolution()
{
  if (inmesh_ == NULL)
  {
    fprintf(stderr,"Inmesh is NULL!\n");
    return CV_ERROR;
  }
  if (outmesh_ == NULL)
  {
    fprintf(stderr,"Outmesh is NULL!\n");
    return CV_ERROR;
  }
  if (AdaptUtils_fix4SolutionTransfer(inmesh_,outmesh_,options.ndof_) != CV_OK)
  {
    fprintf(stderr,"ERROR: Solution was not transferred\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// -----------------------
//  TransferRegions
// -----------------------
int cvMeshSimAdapt::TransferRegions()
{
  if (insurface_mesh_ == NULL)
  {
    fprintf(stderr,"In surfacemesh is NULL!\n");
    return CV_ERROR;
  }
  if (outsurface_mesh_ == NULL)
  {
    fprintf(stderr,"Out surfacemesh is NULL!\n");
    return CV_ERROR;
  }

  if (AdaptUtils_modelFaceIDTransfer(insurface_mesh_,outsurface_mesh_) != CV_OK)
  {
    fprintf(stderr,"ERROR: Regions were not transferred\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// -----------------------
//  WriteCompleteMeshFiles
// -----------------------
int cvMeshSimAdapt::WriteCompleteMeshFiles(char *dirName)
{
  fprintf(stderr,"Not complete yet!\n");
  return CV_OK;
}

// -----------------------
//  WriteAdaptedModel
// -----------------------
int cvMeshSimAdapt::WriteAdaptedModel(char *fileName)
{
  if (outsurface_mesh_ == NULL)
  {
    if (meshobject_ == NULL)
    {
      fprintf(stderr,"Mesh Object is null!\n");
      return CV_ERROR;
    }

    this->GetAdaptedMesh();
  }

  vtkSmartPointer<vtkXMLPolyDataWriter> writer = 
    vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  writer->SetInputData(outsurface_mesh_);
  writer->SetFileName(fileName);
  writer->Update();
  return CV_OK;
}

// -----------------------
//  WriteAdaptedMesh
// -----------------------
int cvMeshSimAdapt::WriteAdaptedMesh(char *fileName)
{
  if (outmesh_ == NULL)
  {
    if (meshobject_ == NULL)
    {
      fprintf(stderr,"Mesh Object is null!\n");
      return CV_ERROR;
    }
    this->GetAdaptedMesh();
  }

  vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer = 
    vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
  writer->SetInputData(outmesh_);
  writer->SetFileName(fileName);
  writer->Update();

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

