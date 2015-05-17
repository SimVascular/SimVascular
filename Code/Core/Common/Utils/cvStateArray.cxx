/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "SimVascular.h" 

#include "cvStateArray.h"


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
