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

/** @file svGCell.cxx
 *  @brief a node of Graphs
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "svGCell.h"

svGCell::svGCell()
{
  this->Parent      = NULL;
  this->Children[0] = NULL;
  this->Children[1] = NULL;
  this->Id       = -1;
  this->GroupId  = -1;
  this->Dir      = -1;
  this->DivergingChild = -1;
  this->AligningChild  = -1;
  this->IsAlign        = -1;
  this->RefAngle = 0.0;
  for (int i=0; i<3; i++)
  {
    this->StartPt[i]  = -1.0;
    this->EndPt[i]    = -1.0;
    this->FrontDir[i] = -1.0;
    this->ZVec[i]   = -1.0;
    this->XVec[i] = -1.0;
  }
  for (int i=0; i<8; i++)
    this->CornerPtIds[i] = -1;
}

svGCell::~svGCell()
{
  if (this->Children[0] != NULL)
  {
    delete this->Children[0];
    this->Children[0] = NULL;
  }
  if (this->Children[1] != NULL)
  {
    delete this->Children[1];
    this->Children[1] = NULL;
  }
}

