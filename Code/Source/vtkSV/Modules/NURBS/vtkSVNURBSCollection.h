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

/**
 *  \class vtkSVNURBSCollection
 *  \brief This is a wrapper around vtkCollection for NURBS
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVNURBSCollection_h
#define vtkSVNURBSCollection_h

#include "vtkCollection.h"
#include "vtkSVNURBSModule.h"

#include <vector>

class vtkSVNURBSObject;

class VTKSVNURBS_EXPORT vtkSVNURBSCollection : public vtkCollection
{
public:
  static vtkSVNURBSCollection *New();
  vtkTypeMacro(vtkSVNURBSCollection,vtkCollection);

  /**
   * Add a NURBS object to the list.
   */
  void AddItem(vtkSVNURBSObject *ds);

  /**
   * Get the next NURBS object in the list.
   */
  vtkSVNURBSObject *GetNextItem();

  /**
   * Get the ith NURBS object in the list.
   */
  vtkSVNURBSObject *GetItem(int i);

  /**
   * Reentrant safe way to get an object in a collection. Just pass the
   * same cookie back and forth.
   */
  vtkSVNURBSObject *GetNextDataObject(vtkCollectionSimpleIterator &cookie);

  int AddPatchConnection(const int patch_0, const int patch_1, const int patch_0_face, const int patch_1_face);

  std::vector<std::vector<int> > GetPatchConnections() {return this->PatchConnections;}
  std::vector<std::vector<int> > GetPatchFaceConnections() {return this->PatchFaceConnections;}

  int GetNumberOfPatchConnections() {return this->PatchConnections.size();}


protected:
  vtkSVNURBSCollection() {}
  ~vtkSVNURBSCollection() {}

  std::vector<std::vector<int> > PatchConnections;
  std::vector<std::vector<int> > PatchFaceConnections;


private:
  // hide the standard AddItem from the user and the compiler.
  void AddItem(vtkObject *o) { this->vtkCollection::AddItem(o); };

  vtkSVNURBSCollection(const vtkSVNURBSCollection&);
  void operator=(const vtkSVNURBSCollection&);
};

#endif
// VTK-HeaderTest-Exclude: vtkNURBSCollection.h
