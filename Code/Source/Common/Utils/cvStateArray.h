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

#ifndef __CVSTATEARRAY_H
#define __CVSTATEARRAY_H


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


typedef char StateT;


#define STATE_PHI_MASK      0x03  // and state with this mask --> phi bits
#define STATE_PHI_INSIDE    0x00
#define STATE_PHI_ON        0x01
#define STATE_PHI_OUTSIDE   0x02
#define STATE_PHI_INVALID   0x03

#define STATE_MINE          0x08

#define STATE_NODE_ACTIVE   0x10
#define STATE_NODE_SWEPT    0x20
#define STATE_NODE_COVERED  0x40


class cvStateArray {

public:
  cvStateArray( int size );
  ~cvStateArray();

  int GetNumElems() { return size_; };
  inline StateT &operator[]( int ix );

  inline int GetSign( int ix );
  inline void SetActive( int ix );
  inline void SetSwept( int ix );
  inline void SetCovered( int ix );
  inline void SweptToCovered();

  int GetNumActive();
  int GetNumCovered();
  void ClearActive();
  void ClearSwept();
  void ClearCovered();

  int GetMemoryUsage();

private:
  StateT *arr_;
  int size_;

};


// ----------
// operator[]
// ----------

StateT &cvStateArray::operator[]( int ix )
{
  assert( ( ix >= 0 ) && ( ix < size_ ) );
  return arr_[ix];
}


// -------
// GetSign
// -------

int cvStateArray::GetSign( int ix )
{
  if ( ( ix < 0 ) || ( ix >= size_ ) ) {
    assert(0);
  }
  switch ( arr_[ix] & STATE_PHI_MASK ) {
  case STATE_PHI_ON:
    return 0;
  case STATE_PHI_INSIDE:
    return -1;
  case STATE_PHI_OUTSIDE:
    return 1;
  }
  assert(0);
}


// ---------
// SetActive
// ---------

void cvStateArray::SetActive( int ix )
{
  if ( ( ix < 0 ) || ( ix >= size_ ) ) {
    return;
  }
  arr_[ix] |= STATE_NODE_ACTIVE;
  return;
}


// --------
// SetSwept
// --------

void cvStateArray::SetSwept( int ix )
{
  if ( ( ix < 0 ) || ( ix >= size_ ) ) {
    return;
  }
  arr_[ix] |= STATE_NODE_SWEPT;
  return;
}


// ----------
// SetCovered
// ----------

void cvStateArray::SetCovered( int ix )
{
  if ( ( ix < 0 ) || ( ix >= size_ ) ) {
    return;
  }
  arr_[ix] |= STATE_NODE_COVERED;
  return;
}


// --------------
// SweptToCovered
// --------------

void cvStateArray::SweptToCovered()
{
  int i;

  for ( i = 0; i < size_; i++ ) {
    if ( arr_[i] & STATE_NODE_SWEPT ) {
      arr_[i] |= STATE_NODE_COVERED;
    }
  }
  return;
}


#endif // __CVSTATEARRAY_H
