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
 *  \class vtkSVLoftNURBSCurve
 *  \brief Filter to take an input set of points and loft a full nurbs
 *  curve using global interpolation techniques
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVLoftNURBSCurve_h
#define vtkSVLoftNURBSCurve_h

#include "vtkSVNURBSModule.h"

#include "vtkPolyDataAlgorithm.h"

#include "vtkSVNURBSCurve.h"

class VTKSVNURBS_EXPORT vtkSVLoftNURBSCurve : public vtkPolyDataAlgorithm
{
public:
  static vtkSVLoftNURBSCurve *New();

  vtkTypeMacro(vtkSVLoftNURBSCurve,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Get and set macro for degree of output curve
  vtkGetMacro(Degree, int);
  vtkSetMacro(Degree, int);
  //@}

  //@{
  /// \brief Set knot span type. Can be 'equal', 'average', or 'derivative'
  vtkSetStringMacro(KnotSpanType);
  vtkGetStringMacro(KnotSpanType);
  //@}
  //@{
  /// \brief Set parametric span type. Can be 'equal', 'chord', or 'centripetal'
  vtkSetStringMacro(ParametricSpanType);
  vtkGetStringMacro(ParametricSpanType);
  //@}

  //@{
  /// \brief Get and set macro for spacing of generated polydata
  vtkGetMacro(PolyDataSpacing, double);
  vtkSetMacro(PolyDataSpacing, double);
  //@}

  //@{
  /// \brief Get and set the the derivatives for start and end. Only used if KnotSpanType = "derivative"
  vtkSetVector3Macro(StartDerivative, double);
  vtkGetVector3Macro(StartDerivative, double);
  vtkSetVector3Macro(EndDerivative, double);
  vtkGetVector3Macro(EndDerivative, double);
  //@}

  //@{
  /// \brief Get macro for the numrbs curve object
  vtkGetObjectMacro(Curve, vtkSVNURBSCurve);
  //@}

  /** \brief Function to get a default set of derivatives if none are given
   *  and a knot span type of derivative is given. */
  static int GetDefaultDerivatives(vtkPoints *points, double D0[3], double DN[3]);

//ETX
protected:
  vtkSVLoftNURBSCurve();
  ~vtkSVLoftNURBSCurve();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **, vtkInformationVector *) override;
  virtual int FillInputPortInformation(int, vtkInformation *) override;

  /** \brief lofts a surface and generates polydata representation. */
  int LoftNURBS(vtkPolyData *input, vtkPolyData *outputPD);

  char *KnotSpanType;
  char *ParametricSpanType;

  int Degree;
  double PolyDataSpacing;
  double StartDerivative[3];
  double EndDerivative[3];

  vtkSVNURBSCurve *Curve;

private:
  vtkSVLoftNURBSCurve(const vtkSVLoftNURBSCurve&);  // Not implemented.
  void operator=(const vtkSVLoftNURBSCurve&);  // Not implemented.
};

#endif
