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

#include "cvIntArrayList.h"
#include "cv_misc_utils.h"


// ------------
// cvIntArrayList
// ------------

cvIntArrayList::cvIntArrayList( int size )
{
  arr_ = new int [size];
  dynamic_ = 1;
  maxSize_ = size;
  num_ = 0;
  head_ = 0;
  tail_ = 0;
  curr_ = 0;
}


// ------------
// cvIntArrayList
// ------------
// Avoid dynamic allocation.  Use a pre-allocated buffer for the
// array.

cvIntArrayList::cvIntArrayList( int *buffer, int size )
{
  arr_ = buffer;
  dynamic_ = 0;
  maxSize_ = size;
  num_ = 0;
  head_ = 0;
  tail_ = 0;
  curr_ = 0;
}


// -----
// Reset
// -----

void cvIntArrayList::Reset()
{
  num_ = 0;
  head_ = 0;
  tail_ = 0;
  curr_ = 0;
}


// -------------
// ~cvIntArrayList
// -------------

cvIntArrayList::~cvIntArrayList()
{
  if ( dynamic_ ) {
    if ( arr_ ) {
      delete arr_;
    }
  }
}


// -------
// Prepend
// -------

int cvIntArrayList::Prepend( int elem )
{
  if ( IsFull() ) {
    return CV_ERROR;
  }
  head_ = ( head_ - 1 + maxSize_ ) % maxSize_;
  arr_[head_] = elem;
  num_++;
  return CV_OK;
}


// ------
// Append
// ------

int cvIntArrayList::Append( int elem )
{
  if ( IsFull() ) {
    return CV_ERROR;
  }
  arr_[tail_] = elem;
  tail_ = ( tail_ + 1 ) % maxSize_;
  num_++;
  return CV_OK;
}


// --------------
// RemoveFromHead
// --------------

int cvIntArrayList::RemoveFromHead( int *elem )
{
  if ( IsEmpty() ) {
    return CV_ERROR;
  }
  *elem = arr_[head_];
  head_ = ( head_ + 1 ) % maxSize_;
  num_--;
  return CV_OK;
}


// --------------
// RemoveFromTail
// --------------

int cvIntArrayList::RemoveFromTail( int *elem )
{
  if ( IsEmpty() ) {
    return CV_ERROR;
  }
  tail_ = ( tail_ - 1 + maxSize_ ) % maxSize_;
  *elem = arr_[tail_];
  num_--;
  return CV_OK;
}


// -------
// IsEmpty
// -------

int cvIntArrayList::IsEmpty()
{
  return ( num_ == 0 );
}


// ------
// IsFull
// ------

int cvIntArrayList::IsFull()
{
  return ( num_ == maxSize_ );
}


// ------------
// InitIterator
// ------------

int cvIntArrayList::InitIterator()
{
  curr_ = head_;
  return CV_OK;
}


// -----------
// GetNextElem
// -----------
// Returns true while element to be retrieved is valid.

int cvIntArrayList::GetNextElem( int *elem )
{
  if ( curr_ == tail_ ) {
    return 0;
  }
  *elem = arr_[curr_];
  curr_ = ( curr_ + 1 ) % maxSize_;
  return 1;
}
