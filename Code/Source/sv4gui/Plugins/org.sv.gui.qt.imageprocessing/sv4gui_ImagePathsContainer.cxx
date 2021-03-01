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

#include "sv4gui_ImagePathsContainer.h"
#include "math.h"

#include <vtkCellLocator.h>
#include <vtkGenericCell.h>

//---------------------------
// sv4guiImagePathsContainer
//---------------------------
//
sv4guiImagePathsContainer::sv4guiImagePathsContainer()
{
  distMeasure_ = 0.0;
}

sv4guiImagePathsContainer::sv4guiImagePathsContainer(const sv4guiImagePathsContainer& other) :BaseData(other)
{
}

//----------------------------
// ~sv4guiImagePathsContainer
//----------------------------
//
sv4guiImagePathsContainer::~sv4guiImagePathsContainer()
{
}

//-------------------
// ClearPathElements
//-------------------
//
void sv4guiImagePathsContainer::ClearPathElements()
{
  pathElements_.clear();
}

//----------------
// AddPathElement
//----------------
//
void sv4guiImagePathsContainer::AddPathElement(sv3::PathElement* pathElement)
{
  pathElements_.push_back(*pathElement);
}

double sv4guiImagePathsContainer::GetDistanceMeasure() const
{
  return distMeasure_;
}

//-----------------
// GetPathElements
//-----------------
//
const std::vector<sv3::PathElement>& 
sv4guiImagePathsContainer::GetPathElements() const
{
  return pathElements_;
}

//-----------------
// GetImageSpacing
//-----------------
//
std::array<double,3>
sv4guiImagePathsContainer::GetImageSpacing()
{
  return imageSpacing_;
}

//-----------------
// SetImageSpacing
//-----------------
//
void sv4guiImagePathsContainer::SetImageSpacing(std::array<double,3>& imageSpacing)
{
  imageSpacing_ = imageSpacing;
}

//-------------------------
// ComputeDistanceMeasure 
//-------------------------
//
void sv4guiImagePathsContainer::ComputeDistanceMeasure()
{
  //std::cout << "========== sv4guiImagePathsContainer::ComputeDistanceMeasure ==========" << std::endl;
  //std::cout << "[ComputeDistanceMeasure] pathElements_.size(): " << pathElements_.size() << std::endl;
  double avgDist = 0.0;
  int numPoints = 0;
  for (auto& pathElem : pathElements_) {
      auto pathPoints = pathElem.GetPathPosPoints();
      //std::cout << "[ComputeDistanceMeasure] pathPoints.size(): " << pathPoints.size() << std::endl;
      for (int i = 0; i < pathPoints.size()-1; i++) {
          double pt1[3] = {pathPoints[i][0], pathPoints[i][1], pathPoints[i][2]}; 
          double pt2[3] = {pathPoints[i+1][0], pathPoints[i+1][1], pathPoints[i+1][2]}; 
          auto dist = sqrt(vtkMath::Distance2BetweenPoints(pt1, pt2));
          avgDist += dist;
          numPoints += 1;
      }
  }

  distMeasure_ = avgDist / numPoints;
  //std::cout << "[ComputeDistanceMeasure] distMeasure_: " << distMeasure_ << std::endl;
}



