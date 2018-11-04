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

#include "vtkSVCenterlineGCell.h"

#include "vtkCellArray.h"
#include "vtkMath.h"
#include "vtkSmartPointer.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVIOUtils.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVCenterlineGCell);

// ----------------------
// Constructor
// ----------------------
vtkSVCenterlineGCell::vtkSVCenterlineGCell()
{
  this->Parent   = NULL;
  this->Id       = -1;
  this->GroupId  = -1;
  this->BranchDir      = -1;
  this->DivergingChild = -1;
  this->AligningChild  = -1;
  this->IsAlign        = -1;
  this->RefAngle       = 0.0;
  for (int i=0; i<3; i++)
  {
    this->StartPt[i]  = -1.0;
    this->EndPt[i]    = -1.0;
    this->BranchVec[i] = -1.0;
    for (int j=0; j<3; j++)
      this->RefDirs[i][j] = -1.0;
  }
  for (int i=0; i<8; i++)
    this->CornerPtIds[i] = -1;
}

// ----------------------
// Constructor
// ----------------------
vtkSVCenterlineGCell::vtkSVCenterlineGCell(int a_Id, int a_GroupId,
                                           vtkSVCenterlineGCell *a_Parent)
{
  this->Parent   = a_Parent;
  this->Id       = a_Id;
  this->GroupId  = a_GroupId;
  this->BranchDir      = -1;
  this->DivergingChild = -1;
  this->AligningChild  = -1;
  this->IsAlign        = -1;
  this->RefAngle       = 0.0;
  for (int i=0; i<3; i++)
  {
    if (a_Parent != NULL)
      this->StartPt[i] = a_Parent->EndPt[i];
    else
      this->StartPt[i] = -1.0;
    this->EndPt[i]    = -1.0;
    this->BranchVec[i] = -1.0;
    for (int j=0; j<3; j++)
      this->RefDirs[i][j] = -1.0;
  }
  for (int i=0; i<8; i++)
    this->CornerPtIds[i] = -1;
}

// ----------------------
// Constructor
// ----------------------
vtkSVCenterlineGCell::vtkSVCenterlineGCell(int a_Id, int a_GroupId,
                                           int a_BranchDir, double a_StartPt[3],
                                           double a_EndPt[3])
{
  this->Parent   = NULL;
  this->Id       = a_Id;
  this->GroupId  = a_GroupId;
  this->BranchDir      = a_BranchDir;
  this->DivergingChild = -1;
  this->AligningChild  = -1;
  this->IsAlign        = -1;
  this->RefAngle       = 0.0;
  for (int i=0; i<3; i++)
  {
    this->StartPt[i] = a_StartPt[i];
    this->EndPt[i]   = a_EndPt[i];
    this->BranchVec[i] = -1.0;
    for (int j=0; j<3; j++)
      this->RefDirs[i][j] = -1.0;
  }
  for (int i=0; i<8; i++)
    this->CornerPtIds[i] = -1;
}

// ----------------------
// Constructor
// ----------------------
vtkSVCenterlineGCell::vtkSVCenterlineGCell(int a_Id, int a_GroupId,
                                           int a_BranchDir)
{
  this->Parent   = NULL;
  this->Id       = a_Id;
  this->GroupId  = a_GroupId;
  this->BranchDir      = a_BranchDir;
  this->DivergingChild = -1;
  this->AligningChild  = -1;
  this->IsAlign        = -1;
  this->RefAngle       = 0.0;
  for (int i=0; i<3; i++)
  {
    this->StartPt[i]  = -1.0;
    this->EndPt[i]    = -1.0;
    this->BranchVec[i] = -1.0;
    for (int j=0; j<3; j++)
      this->RefDirs[i][j] = -1.0;
  }
  for (int i=0; i<8; i++)
    this->CornerPtIds[i] = -1;
}

// ----------------------
// Destructor
// ----------------------
vtkSVCenterlineGCell::~vtkSVCenterlineGCell()
{
  for (int i=0; i<this->Children.size(); i++)
  {
    if (this->Children[i] != NULL)
    {
      this->Children[i]->Delete();
      this->Children[i] = NULL;
    }
  }
}

// ----------------------
// GetBeginningType
// ----------------------
int vtkSVCenterlineGCell::GetBeginningType(int &beginningType, int &splitType)
{
  vtkSVCenterlineGCell *parent = this->Parent;

  if (parent == NULL)
  {
    splitType     = ZERO;
    beginningType = NONE;
  }
  else if (parent->Children.size() == 1)
  {
    beginningType   = NOTHANDLED;
    splitType = UNO;
    vtkErrorMacro("There is only one child of this cell, something wrong in centerlines");
    return SV_ERROR;
  }
  else if (parent->Children.size() == 2)
  {
    splitType = BI;

    vtkSVCenterlineGCell *brother;
    if (parent->Children[0]->Id == this->Id)
      brother = parent->Children[1];
    else
      brother = parent->Children[0];

    if ((brother->BranchDir + this->BranchDir)%2 != 0)
    {
      if ((brother->BranchDir + this->BranchDir) == 1)
        beginningType = C_TET_0;
      else if ((brother->BranchDir + this->BranchDir) == 5)
        beginningType = C_TET_2;
      else
      {
        if (brother->BranchDir == 0 || this->BranchDir == 0)
          beginningType = C_TET_3;
        else
          beginningType = C_TET_1;
      }
    }
    else
    {
      if ((parent->BranchDir + this->BranchDir)%2 == 0)
      {
        if (parent->BranchDir == RIGHT || parent->BranchDir == LEFT)
          beginningType = VERT_WEDGE;
        else
          beginningType = HORZ_WEDGE;
      }
      else
      {
        if (parent->BranchDir == RIGHT || parent->BranchDir == LEFT)
          beginningType = HORZ_WEDGE;
        else
          beginningType = VERT_WEDGE;
      }
    }
  }
  else if (parent->Children.size() == 3)
  {
    splitType = TRI;

    vtkSVCenterlineGCell *sister;
    for (int i=0; i<3; i++)
    {
      if (i != parent->DivergingChild && i != parent->AligningChild)
        sister = parent->Children[i];
    }

    if ((parent->Children[parent->DivergingChild]->BranchDir + sister->BranchDir)%2 != 0 ||
        (parent->Children[parent->AligningChild]->BranchDir + sister->BranchDir)%2 != 0)
    {
      if (parent->Children[parent->AligningChild]->Id == this->Id)
      {
        if ((parent->Children[parent->DivergingChild]->BranchDir + sister->BranchDir) == 1)
          beginningType = C_TET_0;
        else if ((parent->Children[parent->DivergingChild]->BranchDir + sister->BranchDir) == 5)
          beginningType = C_TET_2;
        else
        {
          if (parent->Children[parent->DivergingChild]->BranchDir == 0 || sister->BranchDir == 0)
            beginningType = C_TET_3;
          else
            beginningType = C_TET_1;
        }
      }
      else if (parent->Children[parent->DivergingChild]->Id == this->Id)
      {
        if (sister->BranchDir == RIGHT)
          beginningType = S_TET_2;
        else if (sister->BranchDir == BACK)
          beginningType = S_TET_3;
        else if (sister->BranchDir == LEFT)
         beginningType = S_TET_0;
        else if (sister->BranchDir == FRONT)
         beginningType = S_TET_1;
      }
      else
      {
        if (parent->Children[parent->DivergingChild]->BranchDir == RIGHT)
          beginningType = S_TET_2;
        else if (parent->Children[parent->DivergingChild]->BranchDir == BACK)
          beginningType = S_TET_3;
        else if (parent->Children[parent->DivergingChild]->BranchDir == LEFT)
         beginningType = S_TET_0;
        else if (parent->Children[parent->DivergingChild]->BranchDir == FRONT)
         beginningType = S_TET_1;
      }
    }
    else
    {
      if ((parent->BranchDir + this->BranchDir)%2 == 0)
      {
        if (parent->BranchDir == RIGHT || parent->BranchDir == LEFT)
          beginningType = VERT_WEDGE;
        else
          beginningType = HORZ_WEDGE;
      }
      else
      {
        if (parent->BranchDir == RIGHT || parent->BranchDir == LEFT)
          beginningType = HORZ_WEDGE;
        else
          beginningType = VERT_WEDGE;
      }
    }
  }
  else
  {
    beginningType = NOTHANDLED;
    splitType     = TOOMANY;
    vtkErrorMacro("Cannot currently handle more than a trifurcation");
    return SV_ERROR;
  }


  return SV_OK;
}

// ----------------------
// GetEndType
// ----------------------
int vtkSVCenterlineGCell::GetEndType(int &endType, int &splitType)
{
  if (this->Children.size() == 0)
  {
    endType   = NONE;
    splitType = ZERO;
  }
  else if (this->Children.size() == 1)
  {
    endType   = NOTHANDLED;
    splitType = UNO;
    vtkErrorMacro("There is only one child of this cell, something wrong in centerlines");
    return SV_ERROR;
  }
  else if (this->Children.size() == 2)
  {
    splitType = BI;

    if ((this->Children[this->DivergingChild]->BranchDir + this->Children[this->AligningChild]->BranchDir)%2 != 0)
    {
      if ((this->Children[this->DivergingChild]->BranchDir + this->Children[this->AligningChild]->BranchDir) == 1)
        endType = C_TET_0;
      else if ((this->Children[this->DivergingChild]->BranchDir + this->Children[this->AligningChild]->BranchDir) == 5)
        endType = C_TET_2;
      else
      {
        if (this->Children[this->DivergingChild]->BranchDir == 0 || this->Children[this->AligningChild]->BranchDir == 0)
          endType = C_TET_3;
        else
          endType = C_TET_1;
      }
    }
    else
    {
      if ((this->BranchDir + this->Children[this->DivergingChild]->BranchDir)%2 == 0)
      {
        if (this->BranchDir == RIGHT || this->BranchDir == LEFT)
          endType = VERT_WEDGE;
        else
          endType = HORZ_WEDGE;
      }
      else
      {
        if (this->BranchDir == RIGHT || this->BranchDir == LEFT)
          endType = HORZ_WEDGE;
        else
          endType = VERT_WEDGE;
      }
    }
  }
  else if (this->Children.size() == 3)
  {
    splitType = TRI;

    vtkSVCenterlineGCell *daughter;
    for (int i=0; i<3; i++)
    {
      if (i != this->DivergingChild && i != this->AligningChild)
        daughter = this->Children[i];
    }

    if ((this->Children[this->DivergingChild]->BranchDir + daughter->BranchDir)%2 != 0 ||
        (this->Children[this->AligningChild]->BranchDir + daughter->BranchDir)%2 != 0)
    {
      if ((this->Children[this->DivergingChild]->BranchDir + daughter->BranchDir) == 1)
        endType = C_TET_0;
      else if ((this->Children[this->DivergingChild]->BranchDir + daughter->BranchDir) == 5)
        endType = C_TET_2;
      else
      {
        if (this->Children[this->DivergingChild]->BranchDir == 0 || daughter->BranchDir == 0)
          endType = C_TET_3;
        else
          endType = C_TET_1;
      }
    }
    else
    {
      if ((this->BranchDir + this->Children[this->DivergingChild]->BranchDir)%2 == 0)
      {
        if (this->BranchDir == RIGHT || this->BranchDir == LEFT)
          endType = VERT_WEDGE;
        else
          endType = HORZ_WEDGE;
      }
      else
      {
        if (this->BranchDir == RIGHT || this->BranchDir == LEFT)
          endType = HORZ_WEDGE;
        else
          endType = VERT_WEDGE;
      }
    }
  }
  else
  {
    endType   = NOTHANDLED;
    splitType = TOOMANY;
    vtkErrorMacro("Cannot currently handle more than a trifurcation");
    return SV_ERROR;
  }

  return SV_OK;
}

