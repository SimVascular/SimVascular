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

#include "cv_adapt_utils.h"

#include "vtkXMLUnstructuredGridReader.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkXMLPolyDataReader.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkGradientFilter.h"
#include "vtkDoubleArray.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"

#ifdef WIN32
#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#endif

#include <iostream>

cvTetGenAdapt::cvTetGenAdapt() 
  : cvAdaptObject(KERNEL_TETGEN)
{
  inmesh_  = NULL;
  outmesh_ = NULL;
  insurface_mesh_ = NULL;
  outsurface_mesh_ = NULL;

  options.poly_ = 1;
  options.sn_ = 0;
  options.strategy_ = 1;
  options.ratio_ = 0.2;
  options.nvar_ = 5;
  options.hmax_ = 1;
  options.hmin_ = 1;
  options.timestep_ = 0;
  options.ndof_=5;

  mesher_input_.firstnumber = 0;
  mesher_output_.firstnumber = 0;

  sol_ = NULL;
  error_indicator_ = NULL;
  hessians_ = NULL;
  ybar_ = NULL;
}

cvTetGenAdapt::cvTetGenAdapt( const cvTetGenAdapt& Adapt)
  : cvAdaptObject( KERNEL_TETGEN)
{
  Copy(Adapt);
}


cvTetGenAdapt::~cvTetGenAdapt()
{
  fprintf(stdout,"I'm dying!");
  if (inmesh_ != NULL)
    inmesh_->Delete();
  if (outmesh_ != NULL)
    outmesh_->Delete();
  if (insurface_mesh_ != NULL)
    insurface_mesh_->Delete();
  if (outsurface_mesh_ != NULL)
    outsurface_mesh_->Delete();

  if (sol_ != NULL)
    delete [] sol_;
  if (error_indicator_ != NULL)
    delete [] error_indicator_;
  if (hessians_ != NULL)
    delete [] hessians_;
  if (ybar_ != NULL)
    delete [] ybar_;
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
//  LoadModel
// -----------------------
int cvTetGenAdapt::LoadModel(char *fileName)
{
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

  return CV_OK;
}

// -----------------------
//  LoadMesh
// -----------------------
int cvTetGenAdapt::LoadMesh(char *fileName)
{
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

// -----------------------
//  LoadSolutionFromFile
// -----------------------
int cvTetGenAdapt::LoadSolutionFromFile(char *fileName)
{
  if (sol_ != NULL)
    delete [] sol_;

  readArrayFromFile(fileName,"solution",sol_);

  return CV_OK;
}

// -----------------------
//  LoadErrorFromFile
// -----------------------
int cvTetGenAdapt::LoadErrorFromFile(char *fileName)
{
  if (error_indicator_ != NULL)
    delete [] error_indicator_;

  readArrayFromFile(fileName,"ybar",error_indicator_);

  return CV_OK;
}

// -----------------------
//  LoadHessiansFromFile
// -----------------------
int cvTetGenAdapt::LoadHessiansFromFile(char *fileName)
{
  if (hessians_ != NULL)
    delete [] hessians_;

  readArrayFromFile(fileName,"hessians",hessians_);

  return CV_OK;
}


// -----------------------
//  LoadYbarFromFile
// -----------------------
int cvTetGenAdapt::LoadYbarFromFile(char *fileName)
{
  if (error_indicator_ != NULL)
    delete [] error_indicator_;

  readArrayFromFile(fileName,"error",error_indicator_);

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
  else if (!strncmp(flag,"sn",2)) {
    options.sn_ = (int) value;
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
  else if (!strncmp(flag,"timestep",8)) {
    options.timestep_ = (int) value;
  }
  else if (!strncmp(flag,"ndof",8)) {
    options.ndof_ = (int) value;
  }
  else {
    fprintf(stderr,"Flag given is not a valid adapt option\n");
    return CV_ERROR;
  }
  return CV_OK;
}

// -----------------------
//  SetErrorMetric
// -----------------------
int cvTetGenAdapt::SetErrorMetric(char *solution_file)
{
  //fprintf(stderr,"Check values\n");
  //fprintf(stderr,"Poly: %d\n",options.poly_);
  //fprintf(stderr,"Strategy: %d\n",options.strategy_);
  //fprintf(stderr,"Ratio: %.4f\n",options.ratio_);
  //fprintf(stderr,"NVar: %d\n",options.nvar_);
  //fprintf(stderr,"Hmax: %.4f\n",options.hmax_);
  //fprintf(stderr,"Hmin: %.4f\n",options.hmin_);
  //fprintf(stderr,"timestep: %d\n",options.timestep_);
  //fprintf(stderr,"Ndof: %d\n",options.ndof_);
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
    
    cout<<"\n Reading file:"<<endl;
    cout<<" ..."<<solution_file<<" (for \"solution and error\")"<<endl;
    //cout<<" ..."<<error_indicator_file_<<" (for \""<<error_tag<<"\")"<<endl;

    if(options.strategy_==1) {
      // attaching the solution to the original mesh
      if (this->LoadSolutionFromFile(solution_file) != CV_OK)
      {
        fprintf(stderr,"Error: Error when attaching solution to mesh\n");
        return CV_ERROR;
      }
      if (AdaptUtils_attachArray(sol_,inmesh_,"solution",options.ndof_,options.poly_) != CV_OK)
      {
        fprintf(stderr,"Error: Error when attaching sollution to mesh\n");
        return CV_ERROR;
      }

      fprintf(stderr,"Do we get here!\n");
      // read ybar (and compute/use hessians of ybar) 
      if (this->LoadErrorFromFile(solution_file) != CV_OK)
      {
        fprintf(stderr,"Error: Error when attaching sollution to mesh\n");
        return CV_ERROR;
      }
      if (AdaptUtils_attachArray(error_indicator_,inmesh_,"error",options.nvar_,options.poly_) != CV_OK)
      {
        fprintf(stderr,"Error: Error when attaching sollution to mesh\n");
        return CV_ERROR;
      }
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
      double *hessians;
      if (this->LoadHessiansFromFile(solution_file) != CV_OK)
      {
        fprintf(stderr,"Error: Error when attaching solution to mesh\n");
        return CV_ERROR;
      }
      if (hessians_ != NULL)
	delete [] hessians_;
      double *hessians_ = new double[numPoints*6];
      AdaptUtils_getHessiansFromPhasta(hessians,inmesh_,options.nvar_,hessians_);
      delete [] hessians;
      if (AdaptUtils_attachArray(hessians_,inmesh_,"hessians",6,options.poly_) != CV_OK)
      return 0;
    }
    if (AdaptUtils_setSizeFieldUsingHessians(inmesh_,&mesher_input_,options.ratio_,options.hmax_,options.hmin_) != CV_OK)
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
  //if (sol_ == NULL)
  //{
  //  fprintf(stderr,"ERROR: Solution must be loaded prior to runnin the adaptor\n");
  //  return CV_ERROR;
  //}

  if (AdaptUtils_convertToTetGen(inmesh_,insurface_mesh_,&mesher_input_) != CV_OK)
  {
    fprintf(stderr,"Conversion to TetGen failed\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// -----------------------
//  RunAdaptor
// -----------------------
int cvTetGenAdapt::RunAdaptor()
{
  if (inmesh_ == NULL || insurface_mesh_ == NULL)
  {
    fprintf(stderr,"ERROR: Mesh and model must be loaded prior to running the adaptor\n"); 
    return CV_ERROR;
  }
  //if (sol_ == NULL)
  //{
  //  fprintf(stderr,"ERROR: Solution must be loaded prior to runnin the adaptor\n");
  //  return CV_ERROR;
  //}

  if (AdaptUtils_runAdaptor(&mesher_input_,&mesher_output_) != CV_OK)
  {
    fprintf(stderr,"ERROR: Adapting mesh failed\n");
    return CV_ERROR;
  }

  return CV_OK;
}

// -----------------------
//  PrintStats
// -----------------------
int cvTetGenAdapt::PrintStats()
{
  printf("-- Adaptation Done...\n");
  printf(" Total # of elements: %d\n", outmesh_->GetNumberOfCells());
  printf(" Total # of vertices: %d\n\n", outmesh_->GetNumberOfPoints());
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

  if (AdaptUtils_convertToVTK(outmesh_,outsurface_mesh_,&mesher_output_) != CV_OK)
  {
    fprintf(stderr,"ERROR: Conversion to vtk structures failed");
    return CV_ERROR;
  }
  
  if (AdaptUtils_modelFaceIDTransfer(insurface_mesh_,outsurface_mesh_) != CV_OK)
  {
    fprintf(stderr,"ERROR: Model ID information transfer failed");
    return CV_ERROR;
  }

  return CV_OK;
}

// -----------------------
//  TransferSolution
// -----------------------
int cvTetGenAdapt::TransferSolution()
{
  if (inmesh_ == NULL)
  {
    fprintf(stderr,"ERROR: Need input mesh to be able to transfer solution\n");
    return CV_ERROR;
  }
  if (outmesh_ == NULL)
  {
    fprintf(stderr,"ERROR: Need output mesh to be able to transfer solution\n");
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
//  WriteAdaptedModel
// -----------------------
int cvTetGenAdapt::WriteAdaptedModel(char *fileName)
{
  if (outsurface_mesh_ == NULL)
  {
    fprintf(stderr,"ERROR: Need an existing output surface mesh to write\n");
    return CV_ERROR;
  }
  vtkSmartPointer<vtkXMLPolyDataWriter> pdwriter =
    vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  pdwriter->SetInputData(outsurface_mesh_);
  pdwriter->SetFileName(fileName);
  pdwriter->Write();

  return CV_OK;
}

// -----------------------
//  WriteAdaptedMesh
// -----------------------
int cvTetGenAdapt::WriteAdaptedMesh(char *fileName)
{
  if (outmesh_ == NULL)
  {
    fprintf(stderr,"ERROR: Need an existing output mesh to write\n");
    return CV_ERROR;
  }
  printf("\n Writing out the mesh...\n\n");
  vtkSmartPointer<vtkXMLUnstructuredGridWriter> ugwriter =
    vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
  ugwriter->SetInputData(outmesh_);
  ugwriter->SetFileName(fileName);
  ugwriter->Write();

  return CV_OK;
}

// -----------------------
//  WriteAdaptedSolution
// -----------------------
int cvTetGenAdapt::WriteAdaptedSolution(char *fileName)
{

  double *solution;
  if (AdaptUtils_getAttachedArray(solution,outmesh_,"solution",
	options.ndof_,options.poly_) != CV_OK)
  {
    fprintf(stderr,"ERROR: Could not attach solution to mesh\n");
    return CV_ERROR;
  }
  int numPoints = outmesh_->GetNumberOfPoints();
  writeArrayToFile(fileName,"solution","binary","write",numPoints,
      options.ndof_,options.timestep_,solution);

  delete [] solution;
  return CV_OK;
}




