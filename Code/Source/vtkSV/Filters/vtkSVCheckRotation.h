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
 *  \class vtkSVCheckRotation
 *  \brief This is a class to rotate a polydata to another polydata in space
 *  of the same number of points and cells and do a point-wise comparison
 *  arther the rotation.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVCheckRotation_h
#define vtkSVCheckRotation_h

#include "vtkPolyDataAlgorithm.h"
#include "vtkSVFiltersModule.h" // For export

#include "vtkPolyData.h"

class VTKSVFILTERS_EXPORT vtkSVCheckRotation : public vtkPolyDataAlgorithm
{
public:
  static vtkSVCheckRotation* New();
  void PrintSelf(ostream& os, vtkIndent indent);

  //@{
  /// \brief Set macro for a third polydata to compare angles to
  vtkSetObjectMacro(OriginalPd, vtkPolyData);
  //@}

  //@{
  /// \brief Set macro for the cell id to match between the two polydatas
  vtkSetMacro(CellId, int);
  //@}

protected:
  vtkSVCheckRotation();
  ~vtkSVCheckRotation();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector);

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.
  int MoveCenters(); /**< \brief Function to move centers to same location. */
  int FindAndCheckRotation(); /**< \brief Function to rotate and check points. */
  int CheckAnglesWithOriginal(); /**< \brief Checks angles if OriginalPd provided. */
  int MatchPointOrder(); /**< \brief Changes original pd cells to match mapped cells. */

private:
  vtkSVCheckRotation(const vtkSVCheckRotation&);  // Not implemented.
  void operator=(const vtkSVCheckRotation&);  // Not implemented.

  int CellId;

  vtkPolyData *SourcePd;
  vtkPolyData *TargetPd;
  vtkPolyData *MappedPd;

  vtkPolyData *OriginalPd;
};

#endif
