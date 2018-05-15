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

#include "sv_DataObject.h"

// -------
// cvDataObject
// -------
// Recall that constructors are called top-down in the derivation
// hierarchy.  Setting data_ to NULL here will serve as a signal to
// ShallowCopy that data_ has not been allocated yet.  (Note that this
// does not ensure TYPE COMPATIBILITY between ShallowCopy's input and
// the member data_.  This is why ShallowCopy must be protected.)

cvDataObject::cvDataObject( RepositoryDataT type )
  : cvRepositoryData( type )
{
  data_ = NULL;
}


// --------
// ~cvDataObject
// --------

cvDataObject::~cvDataObject()
{
  if ( data_ != NULL ) {
    data_->Delete();
  }
}


// --------------
// GetMemoryUsage
// --------------

int cvDataObject::GetMemoryUsage()
{
  int sz = 0;

  sz += sizeof( this );
  if ( data_ != NULL ) {
    sz += data_->GetActualMemorySize() * 1024;  // vtk returns kB
  }
  return sz;
}
