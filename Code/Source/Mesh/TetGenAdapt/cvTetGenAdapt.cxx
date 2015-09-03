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

/** @file cvTetGenAdapt.cxx
 *  @brief The implementations of functions in cvTetGenAdapt
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "cvTetGenAdapt.h"
#include "cv_tetgenmesh_utils.h"
#include "cv_mesh_init.h"

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

#ifdef WIN32
#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#endif

#include <iostream>


cvTetGenAdapt::cvTetGenAdapt() 
  : cvAdaptObject(KERNEL_TETGEN)
{
  meshobject_ = NULL;
  inmesh_  = NULL;
  insurface_mesh_ = NULL;
  outmesh_ = NULL;
  outsurface_mesh_ = NULL;

  options.poly_ = 1;
  options.metric_option_ = 1;
  options.strategy_ = 1;
  options.ratio_ = 0.2;
  options.hmax_ = 1;
  options.hmin_ = 1;
  options.instep_ = 0;
  options.outstep_ = 0;
  options.step_incr_ = 1;
  options.sphere_[0] = -1;
  options.sphere_[1] = 0;
  options.sphere_[2] = 0;
  options.sphere_[3] = 0;
  options.sphere_[4] = 1;

  sol_ = NULL;
  hessians_ = NULL;
  ybar_ = NULL;
  errormetric_ = NULL;
}

cvTetGenAdapt::cvTetGenAdapt( const cvTetGenAdapt& Adapt)
  : cvAdaptObject( KERNEL_TETGEN)
{
  Copy(Adapt);
}


cvTetGenAdapt::~cvTetGenAdapt()
{
  if (meshobject_ != NULL)
    gRepository->UnRegister(meshobject_->GetName());

  if (inmesh_ != NULL)
    inmesh_->Delete();
  if (insurface_mesh_ != NULL)
    insurface_mesh_->Delete();

  if (outmesh_ != NULL)
    outmesh_->Delete();
  if (outsurface_mesh_ != NULL)
    outsurface_mesh_->Delete();

  if (sol_ != NULL)
    delete [] sol_;
  if (ybar_ != NULL)
    delete [] ybar_;
  if (hessians_ != NULL)
    delete [] hessians_;
  if (errormetric_ != NULL)
    delete [] errormetric_;
}

cvAdaptObject *cvTetGenAdapt::Copy() const
{
  cvTetGenAdapt *result = new cvTetGenAdapt( *this );
  return result;
}

int cvTetGenAdapt::Copy( const cvAdaptObject& src)
{
  cvTetGenAdapt *adaptPtr;

  adaptPtr = (cvTetGenAdapt *)( &src );

  return CV_OK;
}

// -----------------------
//  CreateInternalMeshObject
// -----------------------
int cvTetGenAdapt::CreateInternalMeshObject(Tcl_Interp *interp,
		char *meshFileName,
		char *solidFileName)
{
  if (meshobject_ != NULL)
  {
    fprintf(stderr,"Cannot create a mesh object, one already exists\n");
    return CV_ERROR;
  }

  char* mesh_name = "/adapt/internal/meshobject";
  if ( gRepository->Exists(mesh_name) ) {
    fprintf(stderr,"Object %s already exists\n",mesh_name);
    return TCL_ERROR;
  }
  cvMeshObject::KernelType newkernel = cvMeshObject::GetKernelType("TetGen");
  meshobject_ = cvMeshSystem::DefaultInstantiateMeshObject( interp,meshFileName,solidFileName);
  if ( meshobject_ == NULL ) {
    fprintf(stderr,"Mesh Object is null after instantiation!\n");
    return CV_ERROR;
  }

  int reg_status = gRepository->Register(mesh_name, meshobject_ );
  // Register the mesh:
  if ( reg_status == 0 ) {
    Tcl_AppendResult( interp, "error registering obj ", mesh_name,
        	      " in repository", (char *)NULL );
    fprintf(stderr,"Error when registering\n");
    delete meshobject_;
    return CV_ERROR;
  }
  meshobject_->SetName(mesh_name);
  Tcl_SetResult( interp, meshobject_->GetName(), TCL_VOLATILE );
  Tcl_CreateCommand( interp, Tcl_GetStringResult(interp), cvMesh_ObjectCmd,
        	     (ClientData)meshobject_, fakeDeletegdscMesh );

  if (solidFileName != NULL)
  {
    if (this->LoadModel(solidFileName) != CV_OK)
    {
      fprintf(stderr,"Error loading solid model in internal object creation\n");
      return CV_ERROR;
    }
  }
  if (meshFileName != NULL)
  {
    if (this->LoadMesh(meshFileName) != CV_OK)
    {
      fprintf(stderr,"Error loading mesh in internal object creation\n");
      return CV_ERROR;
    }
  }

 return CV_OK;
}


// -----------------------
//  LoadModel
// -----------------------
int cvTetGenAdapt::LoadModel(char *fileName)
{
  if (!AdaptUtils_file_exists(fileName))
  {
    fprintf(stderr,"File %s does not exist\n",fileName);
    return CV_ERROR;
  }

  if (insurface_mesh_ != NULL)
    insurface_mesh_->Delete();

  vtkSmartPointer<vtkXMLPolyDataReader> pdreader = 
    vtkSmartPointer<vtkXMLPolyDataReader>::New();

  insurface_mesh_ = vtkPolyData::New();
  pdreader->SetFileName(fileName);
  pdreader->Update();
  insurface_mesh_ ->DeepCopy(pdreader->GetOutput());
  printf("\n-- Loaded Model...\n");
  printf(" Total # of faces: %d\n", insurface_mesh_->GetNumberOfCells());
  printf(" Total # of vertices: %d\n", insurface_mesh_->GetNumberOfPoints());
  insurface_mesh_->BuildLinks();

  if (meshobject_ == NULL)
  {
    fprintf(stderr,"Mesh object must first be created with CreateInternalMeshObject before loading\n");
    return CV_ERROR;
  }
  if (meshobject_->LoadModel(fileName) != CV_OK)
    return CV_ERROR;

  return CV_OK;
}

// -----------------------
//  LoadMesh
// -----------------------
int cvTetGenAdapt::LoadMesh(char *fileName)
{
  if (!AdaptUtils_file_exists(fileName))
  {
    fprintf(stderr,"File %s does not exist\n",fileName);
    return CV_ERROR;
  }

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

  return CV_OK;
}

// ---------------
//  LoadSolution
// ---------------
int cvTetGenAdapt::LoadSolutionFromFile(char *fileName)
{
  if (!AdaptUtils_file_exists(fileName))
  {
    fprintf(stderr,"File %s does not exist\n",fileName);
    return CV_ERROR;
  }

  if (sol_ != NULL)
    delete [] sol_;

  AdaptUtils_readArrayFromFile(fileName,"solution",sol_);

  if (inmesh_ != NULL)
  {
    int nVar = 5;//Number of variables in solution
    if (AdaptUtils_attachArray(sol_,inmesh_,"solution",nVar,options.poly_) != CV_OK)
    {
      fprintf(stderr,"Error: Error when attaching solution to mesh\n");
      return CV_ERROR;
    }
  }
  else
  {
    fprintf(stderr,"Must load a mesh to attach solution to mesh\n");
  }

  return CV_OK;
}

// ---------------
//  LoadYbar
// ---------------
int cvTetGenAdapt::LoadYbarFromFile(char *fileName)
{
  if (!AdaptUtils_file_exists(fileName))
  {
    fprintf(stderr,"File %s does not exist\n",fileName);
    return CV_ERROR;
  }

  if (ybar_ != NULL)
    delete [] ybar_;

  AdaptUtils_readArrayFromFile(fileName,"ybar",ybar_);

  if (inmesh_ != NULL)
  {
    int nVar=5; //Number of variables in ybar
    if (AdaptUtils_attachArray(ybar_,inmesh_,"avg_sols",nVar,options.poly_) != CV_OK)
    {
      fprintf(stderr,"Error: Error when attaching error to mesh\n");
      return CV_ERROR;
    }
  }
  else
  {
    fprintf(stderr,"Must load a mesh to attach ybar to mesh\n");
  }

  return CV_OK;
}

// ---------------
//  LoadHessian
// ---------------
int cvTetGenAdapt::LoadHessianFromFile(char *fileName)
{
  if (!AdaptUtils_file_exists(fileName))
  {
    fprintf(stderr,"File %s does not exist\n",fileName);
    return CV_ERROR;
  }

  if (hessians_ != NULL)
    delete [] hessians_;

  AdaptUtils_readArrayFromFile(fileName,"hessians",hessians_);

  if (inmesh_ != NULL)
  {
    int nVar=9;
    if (AdaptUtils_attachArray(hessians_,inmesh_,"hessians",nVar,options.poly_) != CV_OK)
    {
      fprintf(stderr,"Error: Error when attaching error to mesh\n");
      return CV_ERROR;
    }
  }
  else
  {
    fprintf(stderr,"Must load a mesh to attach hessian to mesh\n");
  }

  return CV_OK;
}

// ---------------
//  ReadSolution
// ---------------
int cvTetGenAdapt::ReadSolutionFromMesh()
{
  if (inmesh_ == NULL)
  {
    fprintf(stderr,"Must load mesh before checking to see if solution exists\n");
    return CV_ERROR;
  }

  if (sol_ != NULL)
    delete [] sol_;

  fprintf(stdout,"Getting solution from step %d to step %d in increments of %d\n",
      options.instep_,options.outstep_,options.step_incr_);
  if (AdaptUtils_averageSolutionsOnMesh(inmesh_,options.instep_,
	options.outstep_,options.step_incr_) != CV_OK)
    return CV_ERROR;

  return CV_OK;
}

// ---------------
//  ReadYbar
// ---------------
int cvTetGenAdapt::ReadYbarFromMesh()
{
  if (inmesh_ == NULL)
  {
    fprintf(stderr,"Must load mesh before checking to see if solution exists\n");
    return CV_ERROR;
  }

  if (ybar_ != NULL)
    delete [] ybar_;
  char ybar_step[80];
  sprintf(ybar_step,"%s_%05i","ybar",options.outstep_);
  if (AdaptUtils_checkArrayExists(inmesh_,0,ybar_step) != CV_OK)
    return CV_ERROR;

  int nVar = 5; //Number of variables in ybar
  if (AdaptUtils_getAttachedArray(ybar_,inmesh_,ybar_step,nVar,
	options.poly_) != CV_OK)
  {
    fprintf(stderr,"Error when retrieving ybar array on mesh\n");
    return CV_ERROR;
  }

  if (inmesh_ != NULL)
  {
    int nVar = 5; //Number of variables in ybar
    if (AdaptUtils_attachArray(ybar_,inmesh_,"avg_sols",nVar,options.poly_) != CV_OK)
    {
      fprintf(stderr,"Error: Error when attaching error to mesh\n");
      return CV_ERROR;
    }
  }

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
int cvTetGenAdapt::SetAdaptOptions(char *flag,double value)
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
  else if (!strncmp(flag,"step_incr",9)) {
    options.step_incr_ = (int) value;
  }
  else if (!strncmp(flag,"metric_option",13)) {
     options.metric_option_ = (int) value;
  }
  else if (!strncmp(flag,"strategy",8)) {
    options.strategy_ = (int) value;
  }
  else if (!strncmp(flag,"ratio",5)) {
    options.ratio_ = (double) value;
  }
  else if (!strncmp(flag,"hmax",4)) {
    options.hmax_ = (double) value;
  }
  else if (!strncmp(flag,"hmin",4)) {
    options.hmin_ = (double) value;
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
int cvTetGenAdapt::CheckOptions() 
{
  fprintf(stderr,"Check values\n");
  fprintf(stderr,"Poly: %d\n",options.poly_);
  fprintf(stderr,"Strategy: %d\n",options.strategy_);
  fprintf(stderr,"Ratio: %.4f\n",options.ratio_);
  fprintf(stderr,"Hmax: %.4f\n",options.hmax_);
  fprintf(stderr,"Hmin: %.4f\n",options.hmin_);

  return CV_OK;
}

// -----------------------
//  SetMetric
// -----------------------
int cvTetGenAdapt::SetMetric(char *input,int option, int strategy)
{
  if (option != -1)
    options.metric_option_ = option;
  if (strategy != -1)
    options.strategy_ = strategy;

  if (inmesh_ == NULL)
  {
    fprintf(stderr,"Error: Mesh must be loaded to set hessians\n");
    return CV_ERROR;
  }
  int numPoints = inmesh_->GetNumberOfPoints();
  
  //Options 1,2, and 3 all use the hessian as adaption metric!
  //Option 4 uses an attached array to the input mesh
  switch(options.metric_option_) {
  case 1 : //Read ybar from solution file (restart), and then calculate hessian using VTK classes!
  case 2 : //Read ybar from vtu mesh, and then calculate hessian using VTK classes!
  case 3 : {  //Read solution from mesh and calculate average solution. Then calculate hessian
      if (options.metric_option_ == 1)
      {
	if (ybar_ == NULL)
	{
	  if (input == NULL)
	    return CV_ERROR;
	  if (this->LoadYbarFromFile(input) != CV_OK)
	    return CV_ERROR;
	}
      }
      else if (options.metric_option_ == 2)
      {
	if (this->ReadYbarFromMesh() != CV_OK)
	  return CV_ERROR;
      }
      else if (options.metric_option_ == 3)
      {
	if (this->ReadSolutionFromMesh() != CV_OK)
	  return CV_ERROR;
      }

      //Compute hessian and attach to mesh!
      if (AdaptUtils_hessiansFromSolution(inmesh_) != CV_OK)
      {
	fprintf(stderr,"Error: Error when calculating hessians from solution\n");
	return CV_ERROR;
      }
      if (AdaptUtils_setSizeFieldUsingHessians(inmesh_,
	    options.ratio_,options.hmax_,options.hmin_,
	    options.sphere_,options.strategy_) != CV_OK)
      {
	  fprintf(stderr,"Error: Error when setting size field with hessians\n");
	  return CV_ERROR;
      }
  }
  break;
  case 4: { //Read some other array from the mesh to set on mesh
      if (input != NULL)
      {
	if (AdaptUtils_checkArrayExists(inmesh_,0,input) != CV_OK)
	{
	  fprintf(stderr,"Given array name is not on input mesh!\n");
	}
      }
      else 
      {
	fprintf(stderr,"Must give name of array to use as metric on mesh\n");
	return CV_ERROR;
      }
      double *tmp;
      if (AdaptUtils_getAttachedArray(tmp,inmesh_,input,1,options.poly_) != CV_OK)
      {
	fprintf(stderr,"Error when retrieving array from mesh!\n");
	return CV_ERROR;

      }
      if (AdaptUtils_attachArray(tmp,inmesh_,"errormetric",1,options.poly_) != CV_OK)
      {
	fprintf(stderr,"Error when attaching array to mesh!\n");
	return CV_ERROR;
      }
      delete [] tmp;
  }
  break;
  default : {
      cout<<"Valid metric option not given!"<<endl;
      cout<<"\nSpecify a correct (adaptation) option (1-4):"<<endl;
      cout<<"\n1: Read Ybar from file and then calculate hessian from";
      cout<<"	ybar component 5 (avg. magnitude of velocity over entire"; 
      cout<<" simulation)"<<endl;
      cout<<"2: Read Ybar from vtu mesh and then calculate hessian from ";
      cout<<"ybar component 5 (avg. magnitude of velocity over entire"; 
      cout<<" simulation)"<<endl; 
      cout<<"3: Read solution from vtu mesh, calculate avg. magnitude of";
      cout<<" velocity over specified timestep range. Must provide"; 
      cout<<" cylinder_results as one vtu with all timesteps. Hessian is";
      cout<<"	then calculated from avg. magnitude of velocity."<<endl;
      cout<<"4: Read array from mesh, and specify mesh metric with this array."<<endl;
      
      return CV_ERROR;
  }
  break;
  }

  return CV_OK;
}

// -----------------------
//  SetUpMesh
// -----------------------
int cvTetGenAdapt::SetupMesh()
{
  if (inmesh_ == NULL || insurface_mesh_ == NULL)
  {
    fprintf(stderr,"ERROR: Mesh and model must be loaded prior to running the adaptor\n"); 
    return CV_ERROR;
  }
  if (meshobject_ == NULL)
  {
    fprintf(stderr,"Must create internal mesh object with CreateInternalMeshObject()\n");
    return CV_ERROR;
  }
  int nVar;
  if (options.strategy_ == 1)
    nVar = 1;
  else if (options.strategy_ == 2)
    nVar = 9;
  if (AdaptUtils_checkArrayExists(inmesh_,0,"errormetric") != CV_OK)
  {
    fprintf(stderr,"Error metric must be incident on mesh. Created in SetMetric\n");
    return CV_ERROR;
  }
  if (AdaptUtils_getAttachedArray(errormetric_,inmesh_,"errormetric",nVar,options.poly_) != CV_OK)
  {
    fprintf(stderr,"Error in getting error metric off mesh\n");
    return CV_ERROR;
  }

  double dummy=0;
  meshobject_->SetVtkPolyDataObject(insurface_mesh_);
  meshobject_->SetInputUnstructuredGrid(inmesh_);
  meshobject_->SetMetricOnMesh(errormetric_,
      options.instep_,options.ratio_,options.hmax_,
      options.hmin_,options.strategy_);

  return CV_OK;
}

// -----------------------
//  RunAdaptor
// -----------------------
int cvTetGenAdapt::RunAdaptor()
{
  if (meshobject_ == NULL)
  {
    fprintf(stderr,"Must create internal mesh object with CreateInternalMeshObject()\n");
    return CV_ERROR;
  }
  if (inmesh_ == NULL || insurface_mesh_ == NULL)
  {
    fprintf(stderr,"ERROR: Mesh and model must be loaded prior to running the adaptor\n"); 
    return CV_ERROR;
  }

  meshobject_->Adapt();

  return CV_OK;
}

// -----------------------
//  PrintStats
// -----------------------
int cvTetGenAdapt::PrintStats()
{
  fprintf(stdout,"TODO\n");
  return CV_OK;
}

// -----------------------
//  GetAdaptedMesh
// -----------------------
int cvTetGenAdapt::GetAdaptedMesh()
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
  meshobject_->GetAdaptedMesh(outmesh_,outsurface_mesh_);
  return CV_OK;
}

// -----------------------
//  TransferSolution
// -----------------------
int cvTetGenAdapt::TransferSolution()
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
  int nVar = 5;// Number of variables in sol
  if (AdaptUtils_fix4SolutionTransfer(inmesh_,outmesh_,options.outstep_) != CV_OK)
  {
    fprintf(stderr,"ERROR: Solution was not transferred\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// -----------------------
//  TransferRegions
// -----------------------
int cvTetGenAdapt::TransferRegions()
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
//  WriteAdaptedModel
// -----------------------
int cvTetGenAdapt::WriteAdaptedModel(char *fileName)
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
//  WriteAdaptedModelFace
// -----------------------
int cvTetGenAdapt::WriteAdaptedModelFace(int faceid, char *fileName)
{
  if (outsurface_mesh_ == NULL)
  {
    if (meshobject_ == NULL)
    {
      fprintf(stderr,"Mesh Object is null!\n");
      return CV_ERROR;
    }

  }
  vtkSmartPointer<vtkPolyData> face = 
    vtkSmartPointer<vtkPolyData>::New();
  if (TGenUtils_GetFacePolyData(faceid,outsurface_mesh_,face)  != CV_OK)
  {
    fprintf(stderr,"Error getting face on output surface mesh\n");
    return CV_ERROR;
  }

  vtkSmartPointer<vtkXMLPolyDataWriter> writer = 
    vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  writer->SetInputData(face);
  writer->SetFileName(fileName);
  writer->Update();
  return CV_OK;
}

// -----------------------
//  WriteAdaptedMesh
// -----------------------
int cvTetGenAdapt::WriteAdaptedMesh(char *fileName)
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
int cvTetGenAdapt::WriteAdaptedSolution(char *fileName)
{
  if (AdaptUtils_checkArrayExists(outmesh_,0,"solution") != CV_OK)
  {
    fprintf(stderr,"Array solution does not exist, must transfer solution prior to writing solution file\n");
    return CV_ERROR;
  }

  if (sol_ != NULL)
    delete [] sol_;

  int nVar = 5; //Number of variables in ybar
  if (AdaptUtils_getAttachedArray(sol_,outmesh_,"solution",nVar,
	options.poly_) != CV_OK)
  {
    fprintf(stderr,"Could not get solution from mesh\n");
    return CV_ERROR;
  }

  int numPoints = outmesh_->GetNumberOfPoints();
  AdaptUtils_writeArrayToFile(fileName,"solution","binary","write",numPoints,
      nVar,options.outstep_,sol_);

  return CV_OK;
}

