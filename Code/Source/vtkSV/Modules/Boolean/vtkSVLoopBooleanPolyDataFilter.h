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
 * \class vtkSVLoopBooleanPolyDataFilter
 *
 * \brief Computes the boundary of the union, intersection, or difference
 * volume computed from the volumes defined by two input surfaces.
 *
 * \details The two surfaces do not need to be manifold, but if they are not,
 * unexpected results may be obtained. The resulting surface is
 * available in the first output of the filter. The second output
 * contains a set of polylines that represent the intersection between
 * the two input surfaces.
 * The filter uses vtkSVLoopIntersectionPolyDataFilter. Must have information
 * about the cells on mesh that the intersection lines touch. Filter assumes
 * this information is given.
 * The ouput result will have data about the Original Surface,
 * BoundaryPoints, Boundary Cells,
 * Free Edges, and Bad Triangles
 *
 * \author Adam Updegrove
 * \author updega2@gmail.com
 * \author UC Berkeley
 * \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVLoopBooleanPolyDataFilter_h
#define vtkSVLoopBooleanPolyDataFilter_h

#include "vtkSVBooleanModule.h" // For export macro

#include "vtkPolyDataAlgorithm.h"

class VTKSVBOOLEAN_EXPORT vtkSVLoopBooleanPolyDataFilter :
        public vtkPolyDataAlgorithm
{
public:

  /// \brief Construct object that computes the boolean surface.
  static vtkSVLoopBooleanPolyDataFilter *New();
  vtkTypeMacro(vtkSVLoopBooleanPolyDataFilter,
               vtkPolyDataAlgorithm);

  // PrintSelf
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Integer describing the number of intersection points and lines
  vtkGetMacro(NumberOfIntersectionPoints, int);
  vtkGetMacro(NumberOfIntersectionLines, int);
  //@}

  //@{
  /// \brief Variable to determine what is output if no intersection occurs.
  /// \details ONLY USED IF NO INTERSECTION BETWEEN SURFACES.
  /// 0 = neither (default), 1 = first, 2 = second, 3 = both
  vtkGetMacro(NoIntersectionOutput, int);
  vtkSetMacro(NoIntersectionOutput, int);
  vtkBooleanMacro(NoIntersectionOutput, int);
  //@}

  /// \brief Union intersection, or difference
  enum OperationType
  {
    VTK_UNION=0,
    VTK_INTERSECTION,
    VTK_DIFFERENCE
  };

  /// \brief Output if no intersection
  enum NoIntersectionOutputType
  {
    VTK_NEITHER=0,
    VTK_FIRST,
    VTK_SECOND,
    VTK_BOTH,
  };

  //@{
  /// \brief  Set the boolean operation to perform. Defaults to union.
  vtkSetClampMacro( Operation, int, VTK_UNION, VTK_DIFFERENCE );
  vtkGetMacro( Operation, int );
  void SetOperationToUnion()
  { this->SetOperation( VTK_UNION ); }
  void SetOperationToIntersection()
  { this->SetOperation( VTK_INTERSECTION ); }
  void SetOperationToDifference()
  { this->SetOperation( VTK_DIFFERENCE ); }
  //@}

  /// \brief Check the status of the filter after update.
  /// \details If the status is zero, there was an error in the operation.
  /// If status is one, everything went smoothly.
  vtkGetMacro(Status, int);

  /// \brief Set the tolerance for geometric tests.
  vtkGetMacro(Tolerance, double);
  vtkSetMacro(Tolerance, double);

protected:
  vtkSVLoopBooleanPolyDataFilter();
  ~vtkSVLoopBooleanPolyDataFilter();

  int RequestData(vtkInformation*, vtkInformationVector**,
                  vtkInformationVector*) override;
  int FillInputPortInformation(int, vtkInformation*) override;

  vtkPolyData *OutputSurface;

  int Operation;
  int NoIntersectionOutput;
  int NumberOfIntersectionPoints;
  int NumberOfIntersectionLines;

  int Status;
  double Tolerance;

  /// brief A class containing the actual implementation. Called during Update
  class Impl;


private:
  vtkSVLoopBooleanPolyDataFilter(const vtkSVLoopBooleanPolyDataFilter&);  // Not implemented
  void operator=(const vtkSVLoopBooleanPolyDataFilter&);  // Not implemented
};

#endif
