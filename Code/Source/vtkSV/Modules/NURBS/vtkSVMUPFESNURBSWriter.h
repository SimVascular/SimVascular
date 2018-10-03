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
 * \class   vtkSVMUPFESNURBSWriter
 * \brief   write ASCII raw file
*/

#ifndef vtkSVMUPFESNURBSWriter_h
#define vtkSVMUPFESNURBSWriter_h

#include "vtkSVNURBSModule.h" // For export macro
#include "vtkSVNURBSObject.h"
#include "vtkWriter.h"

class vtkCellArray;
class vtkPoints;
class vtkPolyData;

class VTKSVNURBS_EXPORT vtkSVMUPFESNURBSWriter : public vtkWriter
{
public:
  static vtkSVMUPFESNURBSWriter *New();
  vtkTypeMacro(vtkSVMUPFESNURBSWriter,vtkWriter);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /**
   * Get the input to this writer.
   */
  vtkSVNURBSObject* GetInput();
  vtkSVNURBSObject* GetInput(int port);
  //@}

  //@{
  /**
   * Specify file name of vtk polygon data file to write.
   */
  vtkSetStringMacro(FileName);
  vtkGetStringMacro(FileName);
  //@}

protected:
  vtkSVMUPFESNURBSWriter();
  ~vtkSVMUPFESNURBSWriter()
  {
    delete[] this->FileName;
  }

  void WriteData() override;

  void WriteMUPFESFile(vtkSVNURBSObject *object);

  char* FileName;

  int FillInputPortInformation(int port, vtkInformation *info) override;

private:
  vtkSVMUPFESNURBSWriter(const vtkSVMUPFESNURBSWriter&);
  void operator=(const vtkSVMUPFESNURBSWriter&);
};

#endif

