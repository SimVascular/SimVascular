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
 * \class   vtkSVCleanUnstructuredGrid
 * \brief   merge duplicate points
 *
 *
 * vtkSVCleanUnstructuredGrid is a filter that takes unstructured grid data as
 * input and generates unstructured grid data as output. vtkSVCleanUnstructuredGrid can
 * merge duplicate points (with coincident coordinates) using the vtkMergePoints object
 * to merge points.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 *  \sa vtkCleanPolyData
*/

#ifndef vtkSVCleanUnstructuredGrid_h
#define vtkSVCleanUnstructuredGrid_h

#include "vtkSVMiscModule.h" //needed for exports

#include "vtkIncrementalPointLocator.h"
#include "vtkUnstructuredGridAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVCleanUnstructuredGrid
  : public vtkUnstructuredGridAlgorithm
{
public:
  static vtkSVCleanUnstructuredGrid* New();

  vtkTypeMacro(vtkSVCleanUnstructuredGrid, vtkUnstructuredGridAlgorithm);
  //@{
  /**
   * \brief Set/Get a spatial locator for speeding the search process. By
   * default an instance of vtkMergePoints is used.
   */
  vtkSetObjectMacro(Locator, vtkIncrementalPointLocator);
  vtkGetObjectMacro(Locator, vtkIncrementalPointLocator);
  //@}

  //@{
  /**
   * \brief By default ToleranceIsAbsolute is false and Tolerance is
   * a fraction of Bounding box diagonal, if true, AbsoluteTolerance is
   * used when adding points to locator (merging)
   */
  vtkSetMacro(ToleranceIsAbsolute,int);
  vtkBooleanMacro(ToleranceIsAbsolute,int);
  vtkGetMacro(ToleranceIsAbsolute,int);
  //@}

  //@{
  /**
   * \brief Specify tolerance in terms of fraction of bounding box length.
   * Default is 0.0.
   */
  vtkSetClampMacro(Tolerance,double,0.0,1.0);
  vtkGetMacro(Tolerance,double);
  //@}

  //@{
  /**
   * \brief Specify tolerance in absolute terms. Default is 1.0.
   */
  vtkSetClampMacro(AbsoluteTolerance,double,0.0,VTK_DOUBLE_MAX);
  vtkGetMacro(AbsoluteTolerance,double);
  //@}

  /**
   * \brief Create default locator. Used to create one when none is specified.
   */
  void CreateDefaultLocator(vtkDataSet *input = 0);

  void PrintSelf(ostream& os, vtkIndent indent) override;

protected:
  vtkSVCleanUnstructuredGrid();
  ~vtkSVCleanUnstructuredGrid();

  double Tolerance;
  double AbsoluteTolerance;
  int ToleranceIsAbsolute;

  vtkIncrementalPointLocator *Locator;

  virtual int RequestData(
    vtkInformation*, vtkInformationVector**, vtkInformationVector*) override;
  virtual int FillInputPortInformation(int port, vtkInformation* info) override;

private:
  vtkSVCleanUnstructuredGrid(const vtkSVCleanUnstructuredGrid&);
  void operator=(const vtkSVCleanUnstructuredGrid&);
};
#endif
