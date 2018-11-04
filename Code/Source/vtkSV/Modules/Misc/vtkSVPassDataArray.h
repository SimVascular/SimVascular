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
 *  \class vtkSVPassDataArray
 *  \brief This filter passes data information from one vtkPolyData to another.
 *  These polydatas do not need to be associated in any way. It uses
 *  vtkPointLocator and vtkCellLocators to find the closest points and pass
 *  the information. It passes the array set with PassArrayName. Will be modified
 *  in the future to pass all data arrays if specified.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVPassDataArray_h
#define vtkSVPassDataArray_h

#include "vtkSVMiscModule.h" // For export

#include "vtkPolyDataAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVPassDataArray : public vtkPolyDataAlgorithm
{
public:
  static vtkSVPassDataArray* New();
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Set name for data array to be used to determine the in between sections
  vtkGetStringMacro(PassArrayName);
  vtkSetStringMacro(PassArrayName);
  //@}

  //@{
  /// \brief Set/get macros for telling whether source is cell or point data
  /// and how the information should be transferred
  vtkGetMacro(PassDataIsCellData, int);
  vtkSetMacro(PassDataIsCellData, int);
  vtkBooleanMacro(PassDataIsCellData, int);
  vtkGetMacro(PassDataToCellData, int);
  vtkSetMacro(PassDataToCellData, int);
  vtkBooleanMacro(PassDataToCellData, int);
  //@}

  //@{
  /// \brief Set/get macros for whether to use cell centroid. Default is true
  /// As false, each point of the cell will be tested and the value returned
  /// the most times from the locator will be used. In the case that multiple
  /// values are returned the same amount, the first is used.
  vtkGetMacro(UseCellCentroid, int);
  vtkSetMacro(UseCellCentroid, int);
  vtkBooleanMacro(UseCellCentroid, int);
  //@}

protected:
  vtkSVPassDataArray();
  ~vtkSVPassDataArray();

  /** \brief Usual filter update
   *  \details Two inputs to filter:
   *  1. The source polydata with the array of name provided.
   *  2. The polydata to map the data array to. */
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  /** \brief Naive implementation to get most reoccuring number in list. Okay
   *  because list size is small. */
  void GetMostOccuringId(vtkIdList *idList, vtkIdType &output);

  /** \brief Used if passing a data array to the points of the target polydata. */
  int PassInformationToPoints(vtkPolyData *sourcePd, vtkPolyData *targetPd,
                              const int sourceIsCellData, vtkDataArray *sourceDataArray,
                              vtkDataArray *targetDataArray);

  /** \brief Used if passing a data array to the cells of the target polydata. */
  int PassInformationToCells(vtkPolyData *sourcePd, vtkPolyData *targetPd,
                             const int sourceIsCellData, const int useCellCentroid,
                             vtkDataArray *sourceDataArray,
                             vtkDataArray *targetDataArray);

  char* PassArrayName;

  vtkDataArray *PassDataArray;
  vtkDataArray *NewDataArray;

  vtkPolyData *SourcePd;
  vtkPolyData *TargetPd;

  int PassDataIsCellData;
  int PassDataToCellData;
  int UseCellCentroid;


private:
  vtkSVPassDataArray(const vtkSVPassDataArray&);  // Not implemented.
  void operator=(const vtkSVPassDataArray&);  // Not implemented.
};

#endif
