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
#include "sv3_PathUtils.h"

#include <vtkCellData.h>
#include <vtkDataSetSurfaceFilter.h>
#include <vtkMath.h>
#include <vtkParametricSpline.h>
#include <vtkThreshold.h>
#include <vtkUnstructuredGrid.h>

using sv3::PathUtils;

static std::string CenterlineIdsArrayName = "CenterlineIds";
static std::string GroupIdsArrayName = "GroupIds";

//----------------------------
// ExtractCenterlinesSections
//----------------------------
// Extract centerline geometry as a list of continuous sections.
//
std::vector<vtkSmartPointer<vtkPolyData>>
PathUtils::ExtractCenterlinesSections(vtkSmartPointer<vtkPolyData>& centerlines)
{
  //std::cout << "========== PathUtils::ExtractCenterlinesSections ========== " << std::endl;
  std::vector<vtkSmartPointer<vtkPolyData>> pathsGeometry;

  // Get centerline IDs used to identify sections. 
  //
  auto ids = centerlines->GetCellData()->GetArray(CenterlineIdsArrayName.c_str());
  if (ids == nullptr) {
      throw std::runtime_error("No '" + CenterlineIdsArrayName + "' cell data array found in centerlines geometry.");
  }

  double vrange[2];
  ids->GetRange(vrange);
  int minId = int(vrange[0]);
  int maxId = int(vrange[1]);
  //std::cout << "[ExtractCenterlinesSections] Max id: " << maxId << std::endl;
  //std::cout << "[ExtractCenterlinesSections] Min id: " << minId << std::endl;

  // Extract sections based on cells with group IDs for each centerline ID.
  //
  for (int cid = minId; cid <= maxId; cid++) {
      auto threshold = vtkSmartPointer<vtkThreshold>::New();
      threshold->SetInputData(centerlines);
      threshold->SetInputArrayToProcess(0, 0, 0, "vtkDataObject::FIELD_ASSOCIATION_CELLS", CenterlineIdsArrayName.c_str());
      threshold->ThresholdBetween(cid, cid);
      threshold->Update();

      auto surfacer = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
      surfacer->SetInputData(threshold->GetOutput());
      surfacer->Update();
      auto centerlinesCidThreshold = surfacer->GetOutput();

      auto groupData = centerlinesCidThreshold->GetCellData()->GetArray(GroupIdsArrayName.c_str());
      double lowerValue = groupData->GetRange()[0];
      double upperValue = groupData->GetRange()[1];

      auto groupThreshold = vtkSmartPointer<vtkThreshold>::New();
      groupThreshold->SetInputData(centerlinesCidThreshold);
      groupThreshold->SetInputArrayToProcess(0, 0, 0, "vtkDataObject::FIELD_ASSOCIATION_CELLS", GroupIdsArrayName.c_str());
      groupThreshold->ThresholdBetween(lowerValue, upperValue);
      groupThreshold->Update();

      auto groupSurfacer = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
      groupSurfacer->SetInputData(groupThreshold->GetOutput());
      groupSurfacer->Update();

      auto groupCenterlines = vtkSmartPointer<vtkPolyData>::New();
      groupCenterlines->DeepCopy(groupSurfacer->GetOutput());
      pathsGeometry.push_back(groupCenterlines);
  }

  return pathsGeometry;
}

//------------------
// SampleLinePoints 
//------------------
// Sample a line at a given number of its points. 
//
// The line points are returned at approximately 'numSamples' points. 
// More points are added if the point tangents change by less than
// 'minAngle'.
//
std::vector<std::array<double,3>>
PathUtils::SampleLinePoints(vtkSmartPointer<vtkPolyData>& polydata, int distMult, double minAngle, double distMeasure)
{
  // std::cout << "========== PathUtils::SampleLinePoints ==========" << std::endl;
  auto points = polydata->GetPoints();
  int numPoints = polydata->GetNumberOfPoints();

  // Compute the length of the line segment.
  double clength = 0.0;
  double pt1[3], pt2[3];
  for (int i = 0; i < numPoints-1; i++) {
      points->GetPoint(i, pt1);
      points->GetPoint(i+1, pt2);
      auto dist = sqrt(vtkMath::Distance2BetweenPoints(pt1, pt2));
      clength += dist;
  }

  // Set the max distance allowed between points. 
  double maxDist = distMult * distMeasure; 

  // Make sure there is at least one sample.
  double numSamples = clength / maxDist;
  if (numSamples < 1.0) { 
      maxDist = clength / 2.0;
  }
  //std::cout << "[SampleLinePoints] numSamples: " << numSamples << std::endl;

  // Sample points along the line adding a point if the change in 
  // tangents is too large or if the distance is > maxDist.
  //
  std::vector<std::array<double,3>> samplePoints;
  double lastPoint[3];
  double lastTangent[3];

  for (int i = 0; i < numPoints-1; i++) {
      points->GetPoint(i, pt1);
      points->GetPoint(i+1, pt2);

      double tangent[3];
      for (int j = 0; j < 3; j++) {
          tangent[j] = pt2[j] - pt1[j];
      }
      vtkMath::Normalize(tangent);
      bool addPoint = false;

      // Add first point.
      if (i == 0) { 
          for (int j = 0; j < 3; j++) {
              lastTangent[j] = tangent[j];
              lastPoint[j] = pt1[j];
          }
          addPoint = true;

      // Add a point if the tangent changes too much or the current 
      // point is > max_dist from the last point.
      } else {
          double dp = vtkMath::Dot(lastTangent, tangent);
          if (dp < minAngle) {
              addPoint = true;
          } else {
              double dist = sqrt(vtkMath::Distance2BetweenPoints(pt1, lastPoint));
              if (dist > maxDist) {
                  addPoint = true;
              }
          }
      }

      if (addPoint) {
          for (int j = 0; j < 3; j++) {
              lastTangent[j] = tangent[j];
              lastPoint[j] = pt1[j];
          }
          samplePoints.push_back(std::array<double,3>{pt1[0], pt1[1], pt1[2]});
      }
  }

  points->GetPoint(numPoints-1, pt1);
  samplePoints.push_back(std::array<double,3>{pt1[0], pt1[1], pt1[2]});

  return samplePoints;
}

