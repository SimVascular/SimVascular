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

#include "vtkSVPolyDataRawReader.h"

#include "vtkByteSwap.h"
#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkErrorCode.h"
#include "vtkFloatArray.h"
#include "vtkIncrementalPointLocator.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkLine.h"
#include "vtkMergePoints.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkSVGlobals.h"

#include <algorithm>
#include <cctype>
#include <stdexcept>
#include <string>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVPolyDataRawReader);

vtkCxxSetObjectMacro(vtkSVPolyDataRawReader, Locator, vtkIncrementalPointLocator);

// ----------------------
// Constructor
// ----------------------
// Construct object with merging set to true.
vtkSVPolyDataRawReader::vtkSVPolyDataRawReader()
{
  this->FileName = NULL;
  this->Merging = 0;
  this->Locator = NULL;

  this->SetNumberOfInputPorts(0);
}

// ----------------------
// Destructor
// ----------------------
vtkSVPolyDataRawReader::~vtkSVPolyDataRawReader()
{
  this->SetFileName(0);
  this->SetLocator(0);
}

// ----------------------
// RequestData
// ----------------------
int vtkSVPolyDataRawReader::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **vtkNotUsed(inputVector),
  vtkInformationVector *outputVector)
{
  vtkInformation *outInfo = outputVector->GetInformationObject(0);
  vtkPolyData *output = vtkPolyData::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));

  // All of the data in the first piece.
  if (outInfo->Get(vtkStreamingDemandDrivenPipeline::UPDATE_PIECE_NUMBER()) > 0)
  {
    return 0;
  }

  if (!this->FileName || *this->FileName == 0)
  {
    vtkErrorMacro(<<"A FileName must be specified.");
    this->SetErrorCode(vtkErrorCode::NoFileNameError);
    return 0;
  }

  // Initialize
  FILE *fp = fopen(this->FileName, "r");
  if (fp == NULL)
  {
    vtkErrorMacro(<< "File " << this->FileName << " not found");
    this->SetErrorCode(vtkErrorCode::CannotOpenFileError);
    return 0;
  }

  vtkPoints *newPts = vtkPoints::New();
  vtkCellArray *newPolys = vtkCellArray::New();

  newPts->Allocate(5000);
  newPolys->Allocate(10000);
  if (!this->ReadRawFile(fp, newPts, newPolys))
  {
    fclose(fp);
    return 0;
  }

  vtkDebugMacro(<< "Read: "
    << newPts->GetNumberOfPoints() << " points, "
    << newPolys->GetNumberOfCells() << " triangles");

  fclose(fp);

  // If merging is on, create hash table and merge points/triangles.
  vtkPoints *mergedPts = newPts;
  vtkCellArray *mergedPolys = newPolys;
  if (this->Merging)
  {
    mergedPts = vtkPoints::New();
    mergedPts->Allocate(newPts->GetNumberOfPoints() /2);
    mergedPolys = vtkCellArray::New();
    mergedPolys->Allocate(newPolys->GetSize());

    vtkSmartPointer<vtkIncrementalPointLocator> locator = this->Locator;
    if (this->Locator == NULL)
    {
      locator.TakeReference(this->NewDefaultLocator());
    }
    locator->InitPointInsertion(mergedPts, newPts->GetBounds());

    int nextCell = 0;
    vtkIdType *pts = 0;
    vtkIdType npts;
    for (newPolys->InitTraversal(); newPolys->GetNextCell(npts, pts);)
    {
      vtkIdType nodes[3];
      for (int i = 0; i < 3; i++)
      {
        double x[3];
        newPts->GetPoint(pts[i], x);
        locator->InsertUniquePoint(x, nodes[i]);
      }

      if (nodes[0] != nodes[1] &&
        nodes[0] != nodes[2] &&
        nodes[1] != nodes[2])
      {
        mergedPolys->InsertNextCell(3, nodes);
      }
      nextCell++;
    }

    newPts->Delete();
    newPolys->Delete();

    vtkDebugMacro(<< "Merged to: "
      << mergedPts->GetNumberOfPoints() << " points, "
      << mergedPolys->GetNumberOfCells() << " triangles");
  }

  output->SetPoints(mergedPts);
  mergedPts->Delete();

  vtkNew(vtkIdList, testCell);
  mergedPolys->GetCell(0, testCell);
  if (testCell->GetNumberOfIds() == 3)
    output->SetPolys(mergedPolys);
  else if (testCell->GetNumberOfIds() == 2)
    output->SetLines(mergedPolys);
  mergedPolys->Delete();

  if (this->Locator)
  {
    this->Locator->Initialize(); //free storage
  }

  output->Squeeze();

  return 1;
}

// ----------------------
// ReadRawFile
// ----------------------
int vtkSVPolyDataRawReader::ReadRawFile(FILE *fp, vtkPoints *newPts,
                                vtkCellArray *newPolys)
{
  vtkDebugMacro(<< "Reading Raw file");

  char  line[256];
  float x[3];
  int   top[2];
  vtkIdType tripts[3];
  vtkIdType linepts[2];

  // header:
  int lineCount = 0;
  if(fscanf(fp, "%d %d\n", top, top+1) != 2)
    throw std::runtime_error("unable to read Raw header");
  lineCount++;
  newPts->SetNumberOfPoints(top[0]);

  try
  {
    // Go into loop, reading points
    for (int i=0; i<top[0]; i++)
    {
      if (!fgets(line, 255, fp))
        throw std::runtime_error("unable to read Raw vertex line.");

      int numItems = sscanf(line, "%f %f %f\n", x, x+1, x+2);
      if (numItems != 3)
      {
        fprintf(stderr,"%d items on vertex line.\n", numItems);
        throw std::runtime_error("unable to read Raw vertex.");
      }
      lineCount++;
      newPts->SetPoint(i, x);

    }

    // Go into loop, reading cells
    for (int i=0; i<top[1]; i++)
    {
      if (!fgets(line, 255, fp))
        throw std::runtime_error("unable to read Raw cell line.");

      int numItems = sscanf(line, "%lld %lld %lld\n", tripts, tripts+1, tripts+2);
      if (numItems == 3)
      {
        newPolys->InsertNextCell(3, tripts);
      }
      else
      {
        numItems = sscanf(line, "%lld %lld\n", linepts, linepts+1);
        if (numItems == 2)
        {
          vtkNew(vtkLine, newLine);
          newLine->GetPointIds()->SetId(0, linepts[0]);
          newLine->GetPointIds()->SetId(1, linepts[1]);
          newPolys->InsertNextCell(newLine);
        }
        else
        {
          fprintf(stderr,"%d items on cell line.\n", numItems);
          throw std::runtime_error("unable to read Raw cell.");
        }
      }

      lineCount++;
      if ((newPolys->GetNumberOfCells() % 5000) == 0)
      {
        this->UpdateProgress((newPolys->GetNumberOfCells()%50000) / 50000.0);
      }
    }
  }
  catch (const std::runtime_error &e)
  {
    vtkErrorMacro("RawReader: error while reading file " <<
      this->FileName << " at line " << lineCount << ": " << e.what());
    return false;
  }

  return true;
}

// ----------------------
// NewDefaultLocator
// ----------------------
/** \brief Specify a spatial locator for merging points. By
 * default an instance of vtkMergePoints is used.
 */
vtkIncrementalPointLocator* vtkSVPolyDataRawReader::NewDefaultLocator()
{
  return vtkMergePoints::New();
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVPolyDataRawReader::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "File Name: "
     <<(this->FileName ? this->FileName : "(none)") << "\n";

  os << indent << "Merging: " <<(this->Merging ? "On\n" : "Off\n");
  os << indent << "Locator: ";
  if (this->Locator)
  {
    this->Locator->PrintSelf(os << endl, indent.GetNextIndent());
  }
  else
  {
    os << "(none)\n";
  }
}
