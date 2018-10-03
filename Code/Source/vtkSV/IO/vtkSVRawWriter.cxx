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

#include "vtkSVRawWriter.h"
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

#if !defined(_WIN32) || defined(__CYGWIN__)
# include <unistd.h> /* unlink */
#else
# include <io.h> /* unlink */
#endif

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVRawWriter);

static char header[]="Visualization Toolkit generated SLA File                                        ";

// ----------------------
// Constructor
// ----------------------
vtkSVRawWriter::vtkSVRawWriter()
{
  this->FileName = NULL;
}

// ----------------------
// WriteData
// ----------------------
void vtkSVRawWriter::WriteData()
{
  vtkPoints *pts;
  vtkCellArray *cells;
  vtkPolyData *input = this->GetInput();

  vtkIdType npts, *index;
  input->BuildLinks();
  input->GetCellPoints(0, npts, index);
  if (npts == 2)
    cells = input->GetLines();
  else if (npts == 3)
    cells = input->GetPolys();
  else
  {
    vtkErrorMacro(<<"Raw file only supports triangles and lines");
    this->SetErrorCode(vtkErrorCode::FileFormatError);
    return;
  }

  pts = input->GetPoints();
  if (pts == NULL || cells == NULL)
  {
    vtkErrorMacro(<<"No data to write!");
    this->SetErrorCode(vtkErrorCode::UnknownError);
    return;
  }

  if (this->FileName == NULL)
  {
    vtkErrorMacro(<< "Please specify FileName to write");
    this->SetErrorCode(vtkErrorCode::NoFileNameError);
    return;
  }

  this->WriteRawFile(pts,cells);
  if (this->ErrorCode == vtkErrorCode::OutOfDiskSpaceError)
  {
    vtkErrorMacro("Ran out of disk space; deleting file: "
                  << this->FileName);
    unlink(this->FileName);
  }
}

// ----------------------
// WriteRawFile
// ----------------------
void vtkSVRawWriter::WriteRawFile(
  vtkPoints *pts, vtkCellArray *cells)
{
  FILE *fp;
  double v[3];
  int top[2];
  vtkIdType npts = 0;
  vtkIdType *indx = 0;

  if ((fp = fopen(this->FileName, "w")) == NULL)
  {
    vtkErrorMacro(<< "Couldn't open file: " << this->FileName);
    this->SetErrorCode(vtkErrorCode::CannotOpenFileError);
    return;
  }
//
//  Write header
//
  vtkDebugMacro("Writing ASCII raw file");

  top[0] = pts->GetNumberOfPoints();
  top[1] = cells->GetNumberOfCells();
  fprintf (fp, "%d %d\n", top[0], top[1]);

  //  Write out triangle polygons.  If not a triangle polygon, report
  //  an error
  vtkNew(vtkIdList, testCell);
  cells->GetCell(0, testCell);
  for (int i=0; i<top[0]; i++)
  {
    pts->GetPoint(i, v);
    if (testCell->GetNumberOfIds() == 2)
      fprintf (fp, "%.6f %.6f %.6f %d\n", v[0], v[1], v[2], 0);
    else
      fprintf (fp, "%.6f %.6f %.6f\n", v[0], v[1], v[2]);
  }
  for (cells->InitTraversal(); cells->GetNextCell(npts,indx); )
  {
    if (npts > 3)
    {
      fclose(fp);
      vtkErrorMacro(<<"Raw file only supports triangles and lines");
      this->SetErrorCode(vtkErrorCode::FileFormatError);
      return;
    }

    if (npts == 3)
    {
      fprintf (fp, "%lld %lld %lld\n", indx[0], indx[1], indx[2]);
    }
    else if (npts == 2)
    {
      fprintf (fp, "%lld %lld\n", indx[0], indx[1]);
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

// ----------------------
// PrintSelf
// ----------------------
void vtkSVRawWriter::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "FileName: "
     << ((this->GetFileName() == NULL) ?
         "(none)" : this->GetFileName()) << std::endl;
  os << indent << "Input: " << this->GetInput() << std::endl;
}

// ----------------------
// GetInput
// ----------------------
vtkPolyData* vtkSVRawWriter::GetInput()
{
  return vtkPolyData::SafeDownCast(this->GetInput(0));
}

// ----------------------
// GetInput
// ----------------------
vtkPolyData* vtkSVRawWriter::GetInput(int port)
{
  return vtkPolyData::SafeDownCast(this->Superclass::GetInput(port));
}

// ----------------------
// FillInputPortInformation
// ----------------------
int vtkSVRawWriter::FillInputPortInformation(int, vtkInformation *info)
{
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkPolyData");
  return 1;
}
