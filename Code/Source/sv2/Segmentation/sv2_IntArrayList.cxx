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

#include "sv_misc_utils.h"

#include "sv2_IntArrayList.h"

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
    return SV_ERROR;
  }
  head_ = ( head_ - 1 + maxSize_ ) % maxSize_;
  arr_[head_] = elem;
  num_++;
  return SV_OK;
}


// ------
// Append
// ------

int cvIntArrayList::Append( int elem )
{
  if ( IsFull() ) {
    return SV_ERROR;
  }
  arr_[tail_] = elem;
  tail_ = ( tail_ + 1 ) % maxSize_;
  num_++;
  return SV_OK;
}


// --------------
// RemoveFromHead
// --------------

int cvIntArrayList::RemoveFromHead( int *elem )
{
  if ( IsEmpty() ) {
    return SV_ERROR;
  }
  *elem = arr_[head_];
  head_ = ( head_ + 1 ) % maxSize_;
  num_--;
  return SV_OK;
}


// --------------
// RemoveFromTail
// --------------

int cvIntArrayList::RemoveFromTail( int *elem )
{
  if ( IsEmpty() ) {
    return SV_ERROR;
  }
  tail_ = ( tail_ - 1 + maxSize_ ) % maxSize_;
  *elem = arr_[tail_];
  num_--;
  return SV_OK;
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
  return SV_OK;
}


// -----------
// GetNextElem
// -----------
// Returns true while element to be retrieved is valid.

int cvIntArrayList::GetNextElem( int *elem )
{
  if ( curr_ == tail_ ) {
    return SV_ERROR;
  }
  *elem = arr_[curr_];
  curr_ = ( curr_ + 1 ) % maxSize_;
  return SV_OK;
}
