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

/**
 * \class vtkSVHausdorffDistance
 *
 * \brief This is a filter to calculate the average and hausdorff distances
 * between two surfaces. The first input is the source polydata and used as
 * the reference polydata. The algorithm processes each point in the second
 * input and calculates the distance of each point to the reference surface.
 * A vtkCellLocator is used for the distance calculation.
 *
 * \author Adam Updegrove
 * \author updega2@gmail.com
 * \author UC Berkeley
 * \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVHausdorffDistance_h
#define vtkSVHausdorffDistance_h

#include "vtkSVMiscModule.h" // For export

#include "vtkPolyDataAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVHausdorffDistance : public vtkPolyDataAlgorithm
{
public:
  static vtkSVHausdorffDistance* New();
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Set/Get name for data array to be used to determine the in between sections
  vtkSetStringMacro(DistanceArrayName);
  vtkGetStringMacro(DistanceArrayName);
  //@}

  //@{
  /// \brief Get macros for hausdorff and average distance found
  vtkGetMacro(HausdorffDistance, double);
  vtkGetMacro(AverageDistance, double);
  vtkGetMacro(MinimumDistance, double);
  //@}

protected:
  vtkSVHausdorffDistance();
  ~vtkSVHausdorffDistance();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  int PrepFilter(); // Prep work
  int RunFilter(); // Run filter operations

  char* DistanceArrayName; // Name of distance data array

  vtkPolyData *SourcePd; // First input to the filter
  vtkPolyData *TargetPd; // Second input to the filter

  double AverageDistance; // The average calculated distance from target to source
  double HausdorffDistance; // The largest distance from all point distances
  double MinimumDistance; // The smallest distance from all point distances

private:
  vtkSVHausdorffDistance(const vtkSVHausdorffDistance&);  // Not implemented.
  void operator=(const vtkSVHausdorffDistance&);  // Not implemented.
};

#endif


