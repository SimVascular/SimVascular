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
 * \class   vtkSVRawWriter
 * \brief   write ASCII raw file
*/

#ifndef vtkSVRawWriter_h
#define vtkSVRawWriter_h

#include "vtkSVIOModule.h" // For export macro
#include "vtkWriter.h"

class vtkCellArray;
class vtkPoints;
class vtkPolyData;

class VTKSVIO_EXPORT vtkSVRawWriter : public vtkWriter
{
public:
  static vtkSVRawWriter *New();
  vtkTypeMacro(vtkSVRawWriter,vtkWriter);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /**
   * Get the input to this writer.
   */
  vtkPolyData* GetInput();
  vtkPolyData* GetInput(int port);
  //@}

  //@{
  /**
   * Specify file name of vtk polygon data file to write.
   */
  vtkSetStringMacro(FileName);
  vtkGetStringMacro(FileName);
  //@}

protected:
  vtkSVRawWriter();
  ~vtkSVRawWriter()
  {
    delete[] this->FileName;
  }

  void WriteData() override;

  void WriteRawFile(
    vtkPoints *pts, vtkCellArray *cells);

  char* FileName;

  int FillInputPortInformation(int port, vtkInformation *info) override;

private:
  vtkSVRawWriter(const vtkSVRawWriter&);
  void operator=(const vtkSVRawWriter&);
};

#endif

