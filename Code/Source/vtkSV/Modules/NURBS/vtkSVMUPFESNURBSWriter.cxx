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

#include "vtkSVMUPFESNURBSWriter.h"
#include "vtkSmartPointer.h"

#include "vtkByteSwap.h"
#include "vtkCellArray.h"
#include "vtkErrorCode.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkTriangle.h"
#include "vtkTriangleStrip.h"
#include "vtkSVGlobals.h"
#include "vtkSVNURBSVolume.h"

#if !defined(_WIN32) || defined(__CYGWIN__)
# include <unistd.h> /* unlink */
#else
# include <io.h> /* unlink */
#endif

vtkStandardNewMacro(vtkSVMUPFESNURBSWriter);

static char header[]="Visualization Toolkit generated SLA File                                        ";

vtkSVMUPFESNURBSWriter::vtkSVMUPFESNURBSWriter()
{
  this->FileName = NULL;
}

void vtkSVMUPFESNURBSWriter::WriteData()
{
  vtkSVNURBSObject *input = this->GetInput();

  if (this->FileName == NULL)
  {
    vtkErrorMacro(<< "Please specify FileName to write");
    this->SetErrorCode(vtkErrorCode::NoFileNameError);
    return;
  }

  this->WriteMUPFESFile(input);
  if (this->ErrorCode == vtkErrorCode::OutOfDiskSpaceError)
  {
    vtkErrorMacro("Ran out of disk space; deleting file: "
                  << this->FileName);
    unlink(this->FileName);
  }
}

void vtkSVMUPFESNURBSWriter::WriteMUPFESFile(vtkSVNURBSObject *object)
{
  FILE *fp;
  double v[3];
  int top[2];
  vtkIdType npts = 0;
  vtkIdType *indx = 0;

  if (!strncmp(object->GetType().c_str(),"Volume",6))
  {
    vtkSVNURBSVolume *volume = vtkSVNURBSVolume::SafeDownCast(object);

    if ((fp = fopen(this->FileName, "w")) == NULL)
    {
      vtkErrorMacro(<< "Couldn't open file: " << this->FileName);
      this->SetErrorCode(vtkErrorCode::CannotOpenFileError);
      return;
    }
  //
  //  Write header
  //
    vtkDebugMacro("Writing ASCII MUPFES file");

    vtkDoubleArray *uKnots = volume->GetUKnotVector();
    vtkDoubleArray *vKnots = volume->GetVKnotVector();
    vtkDoubleArray *wKnots = volume->GetWKnotVector();
    int nuk = uKnots->GetNumberOfTuples();
    int nvk = vKnots->GetNumberOfTuples();
    int nwk = wKnots->GetNumberOfTuples();

    vtkSVControlGrid *controlPoints = volume->GetControlPointGrid();

    int dims[3];
    controlPoints->GetDimensions(dims);
    int np = dims[0];
    int mp = dims[1];
    int lp = dims[2];

    fprintf(fp,"#knotV %d\n", nuk);
    for (int i=0; i<nuk; i++)
      fprintf(fp,"%.6f\n", uKnots->GetTuple1(i));
    fprintf(fp,"#knotV %d\n", nvk);
    for (int i=0; i<nvk; i++)
      fprintf(fp,"%.6f\n", vKnots->GetTuple1(i));
    fprintf(fp,"#knotV %d\n", nwk);
    for (int i=0; i<nwk; i++)
      fprintf(fp,"%.6f\n", wKnots->GetTuple1(i));
    fprintf(fp,"#ctrlPts %d\n", np*mp*lp);
    for (int i=0;i<np; i++)
    {
      for (int j=0; j<mp; j++)
      {
        for (int k=0; k<lp; k++)
        {
          double pw[4];
          controlPoints->GetControlPoint(i, j, k, pw);
          fprintf(fp,"%.6f %.6f %.6f %.6f\n", pw[0], pw[1], pw[2], pw[3]);
        }
      }
    }

    if(fflush(fp))
    {
      fclose(fp);
      this->SetErrorCode(vtkErrorCode::OutOfDiskSpaceError);
      return;
    }
    fclose (fp);
  }
}

//----------------------------------------------------------------------------
void vtkSVMUPFESNURBSWriter::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "FileName: "
     << ((this->GetFileName() == NULL) ?
         "(none)" : this->GetFileName()) << std::endl;
  os << indent << "Input: " << this->GetInput() << std::endl;
}

//----------------------------------------------------------------------------
vtkSVNURBSObject* vtkSVMUPFESNURBSWriter::GetInput()
{
  return vtkSVNURBSObject::SafeDownCast(this->GetInput(0));
}

//----------------------------------------------------------------------------
vtkSVNURBSObject* vtkSVMUPFESNURBSWriter::GetInput(int port)
{
  return vtkSVNURBSObject::SafeDownCast(this->Superclass::GetInput(port));
}

//----------------------------------------------------------------------------
int vtkSVMUPFESNURBSWriter::FillInputPortInformation(int, vtkInformation *info)
{
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkSVNURBSObject");
  return 1;
}
