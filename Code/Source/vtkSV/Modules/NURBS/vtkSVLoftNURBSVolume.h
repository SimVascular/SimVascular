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
 *  \class vtkSVLoftNURBSVolume
 *  \brief Filter to take an input set of points and loft a full nurbs
 *  volume using global interpolation techniques
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVLoftNURBSVolume_h
#define vtkSVLoftNURBSVolume_h

#include "vtkSVNURBSModule.h"

#include "vtkUnstructuredGridAlgorithm.h"

#include "vtkSVNURBSVolume.h"

class VTKSVNURBS_EXPORT vtkSVLoftNURBSVolume : public vtkUnstructuredGridAlgorithm
{
public:
  static vtkSVLoftNURBSVolume *New();

  vtkTypeMacro(vtkSVLoftNURBSVolume,vtkUnstructuredGridAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Get and set macro for degree of output surface
  vtkGetMacro(UDegree, int);
  vtkSetMacro(UDegree, int);
  vtkGetMacro(VDegree, int);
  vtkSetMacro(VDegree, int);
  vtkGetMacro(WDegree, int);
  vtkSetMacro(WDegree, int);
  //@}

  //@{
  /// \brief Set knot span type. Can be 'equal', 'avg', or 'endderiv'
  vtkSetStringMacro(UKnotSpanType);
  vtkGetStringMacro(UKnotSpanType);
  vtkSetStringMacro(VKnotSpanType);
  vtkGetStringMacro(VKnotSpanType);
  vtkSetStringMacro(WKnotSpanType);
  vtkGetStringMacro(WKnotSpanType);
  //@}

  //@{
  /// \brief Set parametric span type. Can be 'equal', 'chord', or 'centripetal'
  vtkSetStringMacro(UParametricSpanType);
  vtkGetStringMacro(UParametricSpanType);
  vtkSetStringMacro(VParametricSpanType);
  vtkGetStringMacro(VParametricSpanType);
  vtkSetStringMacro(WParametricSpanType);
  vtkGetStringMacro(WParametricSpanType);
  //@}

  /// \brief Get the nurbs surface
  vtkGetObjectMacro(Volume, vtkSVNURBSVolume);

  //@{
  /// \brief U spacing in the polydata representation when retrieved (0 - 1)
  vtkGetMacro(UnstructuredGridUSpacing, double);
  vtkSetMacro(UnstructuredGridUSpacing, double);
  //@}

  //@{
  /// \brief V spacing in the polydata representation when retrieved (0 - 1)
  vtkGetMacro(UnstructuredGridVSpacing, double);
  vtkSetMacro(UnstructuredGridVSpacing, double);
  //@}

  //@{
  /// \brief V spacing in the polydata representation when retrieved (0 - 1)
  vtkGetMacro(UnstructuredGridWSpacing, double);
  vtkSetMacro(UnstructuredGridWSpacing, double);
  //@}

  //@{
  /// \brief Get and set object macros for the start derivatives. Should have the
  // same number of values as the input point data in respective direction
  vtkSetObjectMacro(StartUDerivatives, vtkStructuredGrid);
  vtkGetObjectMacro(StartUDerivatives, vtkStructuredGrid);
  vtkSetObjectMacro(StartVDerivatives, vtkStructuredGrid);
  vtkGetObjectMacro(StartVDerivatives, vtkStructuredGrid);
  vtkSetObjectMacro(StartWDerivatives, vtkStructuredGrid);
  vtkGetObjectMacro(StartWDerivatives, vtkStructuredGrid);
  //@}

  //@{
  /// \brief Get and set object macros for the end derivatives. Should have the
  // same number of values as the input point data in respective direction
  vtkSetObjectMacro(EndUDerivatives, vtkStructuredGrid);
  vtkGetObjectMacro(EndUDerivatives, vtkStructuredGrid);
  vtkSetObjectMacro(EndVDerivatives, vtkStructuredGrid);
  vtkGetObjectMacro(EndVDerivatives, vtkStructuredGrid);
  vtkSetObjectMacro(EndWDerivatives, vtkStructuredGrid);
  vtkGetObjectMacro(EndWDerivatives, vtkStructuredGrid);
  //@}

  /** \brief Function to get a default set of derivatives if none are given
   *  and a knot span type of derivative is given. */
  int GetDefaultDerivatives(vtkStructuredGrid *input, const int comp,
                            vtkStructuredGrid *D0out, vtkStructuredGrid *DNout);

//ETX
protected:
  vtkSVLoftNURBSVolume();
  ~vtkSVLoftNURBSVolume();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **, vtkInformationVector *) override;
  virtual int FillInputPortInformation(int, vtkInformation *) override;

 private:
  //Function to run the intersection on intersecting polydatas
  int LoftNURBS(vtkStructuredGrid *input, vtkUnstructuredGrid *outputUG);

  char *UKnotSpanType;
  char *VKnotSpanType;
  char *WKnotSpanType;
  char *UParametricSpanType;
  char *VParametricSpanType;
  char *WParametricSpanType;

  int UDegree;
  int VDegree;
  int WDegree;

  double UnstructuredGridUSpacing;
  double UnstructuredGridVSpacing;
  double UnstructuredGridWSpacing;

  vtkStructuredGrid *InputGrid;
  vtkSVNURBSVolume *Volume;

  vtkStructuredGrid *StartUDerivatives;
  vtkStructuredGrid *StartVDerivatives;
  vtkStructuredGrid *StartWDerivatives;
  vtkStructuredGrid *EndUDerivatives;
  vtkStructuredGrid *EndVDerivatives;
  vtkStructuredGrid *EndWDerivatives;

private:
  vtkSVLoftNURBSVolume(const vtkSVLoftNURBSVolume&);  // Not implemented.
  void operator=(const vtkSVLoftNURBSVolume&);  // Not implemented.
};

#endif
