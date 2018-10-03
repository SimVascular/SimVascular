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
 * \class   vtkSVPERIGEENURBSCollectionWriter
 * \brief   write ASCII raw file
*/

#ifndef vtkSVPERIGEENURBSCollectionWriter_h
#define vtkSVPERIGEENURBSCollectionWriter_h

#include "vtkSVNURBSModule.h" // For export macro
#include "vtkSVNURBSObject.h"
#include "vtkAlgorithm.h"
#include "vtkSVNURBSCollection.h"


class VTKSVNURBS_EXPORT vtkSVPERIGEENURBSCollectionWriter : public vtkAlgorithm
{
public:
  static vtkSVPERIGEENURBSCollectionWriter *New();
  vtkTypeMacro(vtkSVPERIGEENURBSCollectionWriter,vtkAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /**
   * Specify file name of vtk polygon data file to write.
   */
  vtkSetStringMacro(FileName);
  vtkGetStringMacro(FileName);
  //@}

  //@{
  /**
   * Set/get the input to this writer.
   */
  void SetInputData(vtkSVNURBSCollection *input);
  void SetInputData(int index, vtkDataObject *input);
  //@}


protected:
  vtkSVPERIGEENURBSCollectionWriter();
  ~vtkSVPERIGEENURBSCollectionWriter()
  {
    delete[] this->FileName;
  }

  int ProcessRequest(vtkInformation*,
                             vtkInformationVector**,
                             vtkInformationVector*) override;

  // convenience method
  virtual int RequestInformation(vtkInformation* request,
                                 vtkInformationVector** inputVector,
                                 vtkInformationVector* outputVector);

  /**
   * This is called by the superclass.
   * This is the method you should override.
   */
  virtual int RequestData(vtkInformation* request,
                          vtkInformationVector** inputVector,
                          vtkInformationVector* outputVector);

  /**
   * This is called by the superclass.
   * This is the method you should override.
   */
  virtual int RequestUpdateExtent(vtkInformation*,
                                  vtkInformationVector**,
                                  vtkInformationVector*);


  char* FileName;

  int FillInputPortInformation(int port, vtkInformation *info) override;

  vtkSVNURBSCollection *InternalCollection;

private:
  vtkSVPERIGEENURBSCollectionWriter(const vtkSVPERIGEENURBSCollectionWriter&);
  void operator=(const vtkSVPERIGEENURBSCollectionWriter&);
};

#endif

