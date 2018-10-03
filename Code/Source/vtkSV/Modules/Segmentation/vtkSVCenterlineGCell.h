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
 *  \brief a node of Graphs
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVCenterlineGCell_h
#define vtkSVCenterlineGCell_h

#include "vtkSVSegmentationModule.h" // For exports

#include "vtkObject.h"
#include "vtkPolyData.h"

class VTKSVSEGMENTATION_EXPORT vtkSVCenterlineGCell : public vtkObject
{
public:
  static vtkSVCenterlineGCell* New();
  vtkTypeMacro(vtkSVCenterlineGCell, vtkObject);

  //Constructors
  vtkSVCenterlineGCell();
  vtkSVCenterlineGCell(int a_Id,
                       int a_GroupId,
                       vtkSVCenterlineGCell *a_Parent);
  vtkSVCenterlineGCell(int a_Id, int a_GroupId, int a_BranchDir);
  vtkSVCenterlineGCell(int a_Id, int a_GroupId, int a_BranchDir, double a_StartPt[3], double a_EndPt[3]);

  //Destructor
  ~vtkSVCenterlineGCell();

  //Member data
  vtkSVCenterlineGCell *Parent;
  std::vector<vtkSVCenterlineGCell *> Children;
  int Id;
  int GroupId;
  int BranchDir;
  double RefAngle;
  double RefDirs[3][3];
  double StartPt[3];
  double EndPt[3];
  double BranchVec[3];
  int CornerPtIds[8];
  int DivergingChild;
  int AligningChild;
  int IsAlign;

  int GetBeginningType(int &beginningType, int &splitType);
  int GetEndType(int &endType, int &splitType);


private:
  vtkSVCenterlineGCell(const vtkSVCenterlineGCell&); // Not implemented.
  void operator=(const vtkSVCenterlineGCell&); // Not implemented.
};

#endif
