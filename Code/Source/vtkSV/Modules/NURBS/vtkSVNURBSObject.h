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
 *  \class vtkSVNURBSObject
 *  \brief This is a class to represent a NURBS curve
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVNURBSObject_h
#define vtkSVNURBSObject_h

#include "vtkDataObject.h"
#include "vtkSVNURBSModule.h"

#include "vtkCellArray.h"
#include "vtkDenseArray.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkPolyData.h"
#include "vtkSVControlGrid.h"

class VTKSVNURBS_EXPORT vtkSVNURBSObject : public vtkDataObject
{
public:
  vtkTypeMacro(vtkSVNURBSObject,vtkDataObject);

  virtual void DeepCopy(vtkSVNURBSObject *src);

  virtual std::string GetType() = 0;

  //@{
  /**
   * Retrieve an instance of this class from an information object.
   */
  static vtkSVNURBSObject* GetData(vtkInformation* info);
  static vtkSVNURBSObject* GetData(vtkInformationVector* v, int i=0);
  //@}


protected:
  vtkSVNURBSObject();
  ~vtkSVNURBSObject();

private:
  vtkSVNURBSObject(const vtkSVNURBSObject&);  // Not implemented.
  void operator=(const vtkSVNURBSObject&);  // Not implemented.
};

#endif
