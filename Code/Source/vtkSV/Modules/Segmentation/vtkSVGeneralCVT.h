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
 *  \class  vtkSVGeneralCVT
 *  \brief This is class to perform CVT clustering of an input polydata and
 *  generators
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVGeneralCVT_h
#define vtkSVGeneralCVT_h

#include "vtkSVSegmentationModule.h" // For export

#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkIdList.h"
#include "vtkPolyDataAlgorithm.h"

#include <vector>

class VTKSVSEGMENTATION_EXPORT vtkSVGeneralCVT : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVGeneralCVT,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Set/get the CVT generators
  vtkSetObjectMacro(Generators, vtkPolyData);
  vtkGetObjectMacro(Generators, vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set macro for array names used by filter
  vtkGetStringMacro(CVTDataArrayName);
  vtkSetStringMacro(CVTDataArrayName);
  vtkGetStringMacro(PatchIdsArrayName);
  vtkSetStringMacro(PatchIdsArrayName);
  //@}

  //@{
  /// \brief Use a cell-based cvt method
  vtkGetMacro(UseCellArray, int);
  vtkSetMacro(UseCellArray, int);
  //@}

  //@{
  /// \brief Use a point-based cvt method
  vtkGetMacro(UsePointArray, int);
  vtkSetMacro(UsePointArray, int);
  //@}

  //@{
  /// \brief Instead of generator points, use array on generators. Useful
  /// if using cvt method with more than three components
  vtkGetMacro(UseGeneratorsArray, int);
  vtkSetMacro(UseGeneratorsArray, int);
  //@}

  //@{
  /// \brief Instead of computing a surface metric or energy, use the number
  /// of points or cells that got transferred to a new patch in that iteration
  /// as the stopping criteria. Default is on.
  vtkGetMacro(UseTransferredPatchesAsThreshold, int);
  vtkSetMacro(UseTransferredPatchesAsThreshold, int);
  //@}

  //@{
  /// \brief Set a maximum number of iterations that can be run to prevent
  /// an infinite loop.
  vtkGetMacro(MaximumNumberOfIterations, int);
  vtkSetMacro(MaximumNumberOfIterations, int);
  //@}

  //@{
  /// \brief Set a maximum number of iterations that can be run to prevent
  /// an infinite loop.
  vtkGetMacro(NoInitialization, int);
  vtkSetMacro(NoInitialization, int);
  //@}

  //@{
  /// \brief Set a threshold criteria. Default is 2 transferred patchs.
  vtkGetMacro(Threshold, double);
  vtkSetMacro(Threshold, double);
  //@}

  //@{
  /// \brief Set a threshold criteria. Default is 2 transferred patchs.
  vtkGetObjectMacro(FixedIdsList, vtkIdList);
  vtkSetObjectMacro(FixedIdsList, vtkIdList);
  //@}

protected:
  vtkSVGeneralCVT();
  ~vtkSVGeneralCVT();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  // Pure virtual functions to be filled by derived class
  virtual int InitializeConnectivity() = 0;
  virtual int InitializeGenerators() = 0;
  virtual int GetClosestGenerator(const int evalId, int &newGenerator) = 0;
  virtual int ComputeSurfaceMetric(double &evalMetric) = 0;
  virtual int UpdateConnectivity(const int evalId, const int oldGenerator, const int newGenerator) = 0;
  virtual int UpdateGenerators() = 0;
  virtual int IsBoundaryCell(const int cellId) = 0;

  vtkPolyData *WorkPd; // Polydata used during filter processing
  vtkPolyData *Generators; // Polydata used during filter processing
  vtkPolyData *WorkGenerators; // Polydata used during filter processing

  vtkDataArray *CVTDataArray;  // Array on input containing info to patch
  vtkDataArray *GeneratorsArray;  // If using array on generators, the array
  vtkIntArray    *PatchIdsArray; // Patch ids array
  vtkIdList      *FixedIdsList;

  std::vector<int> FixedIds;

  char *CVTDataArrayName; // Array name on input with data to patch
  char *PatchIdsArrayName; // Array containing patch id info

  int UsePointArray; // Use point info
  int UseCellArray; // Use cell info
  int UseGeneratorsArray; // Use the array on the generator
  int UseTransferredPatchesAsThreshold; // Determine when to stop based on how elements switch generators

  double Threshold; // Threshold to stop at
  int MaximumNumberOfIterations; // Max iterations
  int NoInitialization; // No generator initialization, useful if array already set on input


private:
  vtkSVGeneralCVT(const vtkSVGeneralCVT&);  // Not implemented.
  void operator=(const vtkSVGeneralCVT&);  // Not implemented.

};

#endif  // vtkSVGeneralCVT_h
