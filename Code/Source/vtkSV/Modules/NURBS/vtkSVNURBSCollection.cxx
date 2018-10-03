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

#include "vtkSVNURBSCollection.h"
#include "vtkObjectFactory.h"

#include "vtkSVNURBSObject.h"
#include "vtkSVGlobals.h"

#include <algorithm>

vtkStandardNewMacro(vtkSVNURBSCollection);

/**
 * Add a NURBS object to the list.
 */
void vtkSVNURBSCollection::AddItem(vtkSVNURBSObject *ds)
{
  this->vtkCollection::AddItem(ds);
}

/**
 * Get the next NURBS object in the list.
 */
vtkSVNURBSObject *vtkSVNURBSCollection::GetNextItem()
{
  return static_cast<vtkSVNURBSObject *>(this->GetNextItemAsObject());
}

/**
 * Get the ith NURBS object in the list.
 */
vtkSVNURBSObject *vtkSVNURBSCollection::GetItem(int i)
{
  return static_cast<vtkSVNURBSObject *>(this->GetItemAsObject(i));
}

/**
 * Reentrant safe way to get an object in a collection. Just pass the
 * same cookie back and forth.
 */
vtkSVNURBSObject *vtkSVNURBSCollection::GetNextDataObject(vtkCollectionSimpleIterator &cookie)
{
  return static_cast<vtkSVNURBSObject *>(this->GetNextItemAsObject(cookie));
}

int vtkSVNURBSCollection::AddPatchConnection(const int patch_0, const int patch_1, const int patch_0_face, const int patch_1_face)
{
  std::vector<int> potConnection;
  potConnection.push_back(patch_0);
  potConnection.push_back(patch_1);
  std::sort(potConnection.begin(), potConnection.end());

  int addConnection = 1;
  for (int i=0; i<this->PatchConnections.size(); i++)
  {
    if (this->PatchConnections[i] == potConnection)
      addConnection = 0;
  }

  if (addConnection)
  {
    this->PatchConnections.push_back(potConnection);
    std::vector<int> newFaceConnection;
    if (potConnection[0] == patch_0)
    {
      newFaceConnection.push_back(patch_0_face);
      newFaceConnection.push_back(patch_1_face);
    }
    else
    {
      newFaceConnection.push_back(patch_1_face);
      newFaceConnection.push_back(patch_0_face);
    }
    this->PatchFaceConnections.push_back(newFaceConnection);
  }

  return SV_OK;
}
