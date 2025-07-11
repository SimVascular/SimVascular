/*=========================================================================

  Program:   VMTK
  Module:    $RCSfile: vtkvmtkStencil.cxx,v $
  Language:  C++
  Date:      $Date: 2006/04/06 16:46:44 $
  Version:   $Revision: 1.5 $

  Copyright (c) Luca Antiga, David Steinman. All rights reserved.
  See LICENCE file for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm
  for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "vtkvmtkStencil.h"
#include "vtkvmtkConstants.h"
#include "vtkObjectFactory.h"
#include <math.h>


vtkvmtkStencil::vtkvmtkStencil()
  {
  this->Weights = nullptr;
  this->CenterWeight = nullptr;
  this->NumberOfComponents = 1;
  this->WeightScaling = 1;
  this->NegateWeights = 1;
  }

vtkvmtkStencil::~vtkvmtkStencil()
  {
  if (this->CenterWeight != nullptr)
    {
    delete[] this->CenterWeight;
    this->CenterWeight = nullptr;
    }

  if (this->Weights != nullptr)
    {
    delete[] this->Weights;
    this->Weights = nullptr;
    }
  }

void vtkvmtkStencil::ChangeWeightSign()
{
  if (!this->NegateWeights)
    {
    return;
    }

  for (int j=0; j<this->NumberOfComponents*this->NPoints; j++)
    {
    this->Weights[j] *= -1.0;
    }
}

void vtkvmtkStencil::ResizePointList(vtkIdType ptId, int size)
  {
  this->Superclass::ResizePointList(ptId,size);

  double *weights;

  weights = new double[this->NumberOfComponents*this->NPoints];
  memcpy(weights, this->Weights,this->NumberOfComponents*this->NPoints*sizeof(double));
  delete [] this->Weights;
  this->Weights = weights;
}

void vtkvmtkStencil::ScaleWeights(double factor)
  {
  vtkIdType j;

  if (!this->WeightScaling)
    {
    return;
    }

  if (fabs(factor)<VTK_VMTK_DOUBLE_TOL)
    {
    for (j=0; j<this->NumberOfComponents*this->NPoints; j++)
      {
      this->Weights[j] = 0.0;
      }
    for (j=0; j<this->NumberOfComponents; j++)
      {
      this->CenterWeight[j] = 0.0;
      }
    return;
    }
  else if (fabs(factor)>VTK_VMTK_LARGE_DOUBLE)
    {
    for (j=0; j<this->NumberOfComponents*this->NPoints; j++)
      {
      this->Weights[j] = VTK_VMTK_LARGE_DOUBLE;
      }
    for (j=0; j<this->NumberOfComponents; j++)
      {
      this->CenterWeight[j] = VTK_VMTK_LARGE_DOUBLE;
      }
    return;
    }

  for (j=0; j<this->NumberOfComponents*this->NPoints; j++)
    {
    this->Weights[j] *= factor;
    if (fabs(this->Weights[j])<VTK_VMTK_DOUBLE_TOL)
      {
      this->Weights[j] = 0.0;
      }
    }

  for (j=0; j<this->NumberOfComponents; j++)
    {
    this->CenterWeight[j] *= factor;
    if (fabs(this->CenterWeight[j])<VTK_VMTK_DOUBLE_TOL)
      {
      this->CenterWeight[j] = 0.0;
      }
    }

  }

void vtkvmtkStencil::DeepCopy(vtkvmtkItem *src)
  {
  this->Superclass::DeepCopy(src);

  vtkvmtkStencil* stencilSrc = vtkvmtkStencil::SafeDownCast(src);

  if (stencilSrc==nullptr)
    {
    vtkErrorMacro(<<"Trying to deep copy a non-stencil item");
    }

  if (this->CenterWeight != nullptr)
    {
    delete[] this->CenterWeight;
    this->CenterWeight = nullptr;
    }

  if (this->Weights != nullptr)
    {
    delete[] this->Weights;
    this->Weights = nullptr;
    }

  this->NumberOfComponents = stencilSrc->NumberOfComponents;

  if (stencilSrc->NPoints>0)
    {
    this->Weights = new double[stencilSrc->NumberOfComponents*stencilSrc->NPoints];
    memcpy(this->Weights, stencilSrc->Weights, this->NumberOfComponents*this->NPoints * sizeof(double));
    }

  this->CenterWeight = new double[stencilSrc->NumberOfComponents];
  memcpy(this->CenterWeight, stencilSrc->CenterWeight, this->NumberOfComponents * sizeof(double));

  this->WeightScaling = stencilSrc->WeightScaling;
  }

