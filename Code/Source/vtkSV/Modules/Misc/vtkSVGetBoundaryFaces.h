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
 *  \class vtkSVGetBoundaryFaces
 *  \brief Get boundary faces separated by feature edges
 *  from poldata and label them with integers
 *  \details
 *  vtkSVGetBoundaryFaces is a filter to extract the boundary surfaces of a model,
 *  separate the surace into multiple regions and number each region. It is
 *  similar to using vtkPolyDataNormals with SplittingOn and then a
 *  vtkConnectivityFilter.
 *
 *  \note See Also vtkExtractEdges
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVGetBoundaryFaces_h
#define vtkSVGetBoundaryFaces_h

#include "vtkSVMiscModule.h" // For export

#include "vtkDoubleArray.h"
#include "vtkFeatureEdges.h"
#include "vtkPolyDataAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVGetBoundaryFaces : public vtkPolyDataAlgorithm
{
public:
  static vtkSVGetBoundaryFaces* New();
  vtkTypeMacro(vtkSVGetBoundaryFaces, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  // \brief Specify the feature angle for extracting feature edges.
  vtkGetMacro(FeatureAngle,double);
  vtkSetMacro(FeatureAngle,double);
  //@}

  //@{
  // \brief Get the number of regions created.
  vtkGetMacro(NumberOfRegions,int);
  //@}

  //@{
  // \brief The name given to the regions created
  vtkSetStringMacro(RegionIdsArrayName);
  vtkGetStringMacro(RegionIdsArrayName);
  //@}

  //@{
  // \brief Specify if boundary edges should be extracted.
  vtkGetMacro(BoundaryEdges, int);
  vtkSetMacro(BoundaryEdges, int);
  //@}

  //@{
  // \brief Specify if manifold edges should be extracted.
  vtkGetMacro(ManifoldEdges, int);
  vtkSetMacro(ManifoldEdges, int);
  //@}

  //@{
  // \brief Specify if non-manifold edges should be extracted.
  vtkGetMacro(NonManifoldEdges, int);
  vtkSetMacro(NonManifoldEdges, int);
  //@}

  //@{
  // \brief Specify if feature edges should be extracted.
  vtkGetMacro(FeatureEdges, int);
  vtkSetMacro(FeatureEdges, int);
  //@}

  //@{
  // \brief Specify if largest region should be extracted
  vtkGetMacro(ExtractLargestRegion, int);
  vtkSetMacro(ExtractLargestRegion, int);
  //@}

protected:
  vtkSVGetBoundaryFaces();
  ~vtkSVGetBoundaryFaces();

  double FeatureAngle;
  int NumberOfRegions;
  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  /** \brief Function to flood fill region fast. */
  void FindBoundaryRegion(int reg,int start, double &area);

  /** \brief Function to flood fill region slower, but is necessary close
   *  to boundaries to make sure it doesn't step over boundary. */
  void FindBoundaryRegionTipToe(int reg, double &area);

  /** \brief Initializes boundary arrays. */
  void SetBoundaryArrays();

  /** \brief function to add currnet cell area to full area.
   *  \param cellId cell whose are to be computed.
   *  \param area area which will be updated with cell area. */
  int  AddCellArea(const int cellId, double &area);

  char *RegionIdsArrayName;

  vtkIntArray *NewScalars;
  vtkDoubleArray *RegionAreas;
  vtkPolyData *WorkPd;
  vtkPolyData *BoundaryLines;
  vtkIntArray *BoundaryPointArray;
  vtkIntArray *BoundaryCellArray;
  vtkIdList *CheckCells;
  vtkIdList *CheckCells2;
  vtkIdList *CheckCellsCareful;
  vtkIdList *CheckCellsCareful2;

  // dynamic allocated arrays
  int *checked;
  int *checkedcarefully;
  int *pointMapper;

  // Feature edges options
  int BoundaryEdges;
  int ManifoldEdges;
  int NonManifoldEdges;
  int FeatureEdges;

  int ExtractLargestRegion;

private:
  vtkSVGetBoundaryFaces(const vtkSVGetBoundaryFaces&);  // Not implemented.
  void operator=(const vtkSVGetBoundaryFaces&);  // Not implemented.
};

#endif
