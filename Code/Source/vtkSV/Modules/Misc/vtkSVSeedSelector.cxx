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

#include "vtkSVSeedSelector.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkGlyph3D.h"
#include "vtkIntArray.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkPolyLine.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPolyDataMapper.h"
#include "vtkProperty.h"
#include "vtkIdList.h"
#include "vtkCellData.h"
#include "vtkCleanPolyData.h"
#include "vtkDoubleArray.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkSphereSource.h"
#include "vtkThreshold.h"
#include "vtkTriangleFilter.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVIOUtils.h"
#include "vtkSVGlobals.h"

vtkStandardNewMacro(vtkSVSeedSelector);

vtkSVSeedSelector::vtkSVSeedSelector()
{
  this->SurfacePd   = vtkPolyData::New();

  this->SourceSeedIds = vtkIdList::New();
  this->TargetSeedIds = vtkIdList::New();
}

vtkSVSeedSelector::~vtkSVSeedSelector()
{
  if (this->SurfacePd != NULL)
  {
    this->SurfacePd->Delete();
    this->SurfacePd = NULL;
  }
  if (this->SourceSeedIds != NULL)
  {
    this->SourceSeedIds->Delete();
    this->SourceSeedIds = NULL;
  }
  if (this->TargetSeedIds != NULL)
  {
    this->TargetSeedIds->Delete();
    this->TargetSeedIds = NULL;
  }
}

void vtkSVSeedSelector::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}
