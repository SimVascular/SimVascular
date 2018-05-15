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

#include "SimVascular.h"

#include "sv2_StateArray.h"


// ----------
// cvStateArray
// ----------

cvStateArray::cvStateArray( int size )
{
  int i;

  arr_ = new StateT [size];
  size_ = size;
  for ( i = 0; i < size_; i++ ) {
    arr_[i] = 0;
  }
}


// -----------
// ~cvStateArray
// -----------

cvStateArray::~cvStateArray()
{
  delete [] arr_;
}


// ------------
// GetNumActive
// ------------

int cvStateArray::GetNumActive()
{
  int i;
  int num = 0;

  for ( i = 0; i < size_; i++ ) {
    if ( arr_[i] & STATE_NODE_ACTIVE ) {
      num++;
    }
  }
  return num;
}


// -------------
// GetNumCovered
// -------------

int cvStateArray::GetNumCovered()
{
  int i;
  int num = 0;

  for ( i = 0; i < size_; i++ ) {
    if ( arr_[i] & STATE_NODE_COVERED ) {
      num++;
    }
  }
  return num;
}


// -----------
// ClearActive
// -----------

void cvStateArray::ClearActive()
{
  int i;

  for ( i = 0; i < size_; i++ ) {
    arr_[i] &= ~STATE_NODE_ACTIVE;
  }
}


// ----------
// ClearSwept
// ----------

void cvStateArray::ClearSwept()
{
  int i;

  for ( i = 0; i < size_; i++ ) {
    arr_[i] &= ~STATE_NODE_SWEPT;
  }
}


// ------------
// ClearCovered
// ------------

void cvStateArray::ClearCovered()
{
  int i;

  for ( i = 0; i < size_; i++ ) {
    arr_[i] &= ~STATE_NODE_COVERED;
  }
}


// --------------
// GetMemoryUsage
// --------------

int cvStateArray::GetMemoryUsage()
{
  int sz = 0;

  sz += sizeof(this);
  sz += size_ * sizeof(StateT);

  return sz;
}
