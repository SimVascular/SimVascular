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

#include "SimVascular.h"
#include "svLSetExports.h" // For exports

class SV_EXPORT_LSET cvStateArray {

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
  return 0;
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
