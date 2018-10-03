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

#include "vtkSVPERIGEENURBSCollectionWriter.h"
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
#include "vtkSVIOUtils.h"
#include "vtkSVNURBSCollection.h"
#include "vtkSVPERIGEENURBSWriter.h"
#include "vtkStreamingDemandDrivenPipeline.h"

#if !defined(_WIN32) || defined(__CYGWIN__)
# include <unistd.h> /* unlink */
#else
# include <io.h> /* unlink */
#endif

vtkStandardNewMacro(vtkSVPERIGEENURBSCollectionWriter);

static char header[]="Visualization Toolkit generated SLA File                                        ";

vtkSVPERIGEENURBSCollectionWriter::vtkSVPERIGEENURBSCollectionWriter()
{
  this->FileName = NULL;
  this->InternalCollection = NULL;
}

void vtkSVPERIGEENURBSCollectionWriter::SetInputData(vtkSVNURBSCollection *input)
{
  int numItems = input->GetNumberOfItems();

  this->SetNumberOfInputPorts(numItems);
  for (int i=0; i<numItems; i++)
  {
    this->AddInputDataInternal(i, input->GetItem(i));
  }

  this->InternalCollection = input;
}

void vtkSVPERIGEENURBSCollectionWriter::SetInputData(int index, vtkDataObject *input)
{
  this->SetInputDataInternal(index, input);
}

//----------------------------------------------------------------------------
int vtkSVPERIGEENURBSCollectionWriter::ProcessRequest(vtkInformation* request,
                                         vtkInformationVector** inputVector,
                                         vtkInformationVector* outputVector)
{
  // generate the data
  if(request->Has(vtkDemandDrivenPipeline::REQUEST_DATA()))
  {
    return this->RequestData(request, inputVector, outputVector);
  }

  if(request->Has(vtkStreamingDemandDrivenPipeline::REQUEST_UPDATE_EXTENT()))
  {
    return this->RequestUpdateExtent(request, inputVector, outputVector);
  }

  // execute information
  if(request->Has(vtkDemandDrivenPipeline::REQUEST_INFORMATION()))
  {
    return this->RequestInformation(request, inputVector, outputVector);
  }

  return this->Superclass::ProcessRequest(request, inputVector, outputVector);
}

//----------------------------------------------------------------------------
int vtkSVPERIGEENURBSCollectionWriter::RequestUpdateExtent(
  vtkInformation* vtkNotUsed(request),
  vtkInformationVector** inputVector,
  vtkInformationVector* vtkNotUsed(outputVector))
{
  int numInputPorts = this->GetNumberOfInputPorts();
  for (int i=0; i<numInputPorts; i++)
  {
    int numInputConnections = this->GetNumberOfInputConnections(i);
    for (int j=0; j<numInputConnections; j++)
    {
      vtkInformation* inputInfo = inputVector[i]->GetInformationObject(j);
      inputInfo->Set(vtkStreamingDemandDrivenPipeline::EXACT_EXTENT(), 1);
    }
  }
  return 1;
}

//----------------------------------------------------------------------------
int vtkSVPERIGEENURBSCollectionWriter::RequestInformation(
  vtkInformation* vtkNotUsed(request),
  vtkInformationVector** vtkNotUsed(inputVector),
  vtkInformationVector* vtkNotUsed(outputVector))
{
  // do nothing let subclasses handle it
  return 1;
}


int vtkSVPERIGEENURBSCollectionWriter::RequestData(
                                 vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
 int numInputs = this->GetNumberOfInputPorts();

  if (this->FileName == NULL)
  {
    vtkErrorMacro(<< "Please specify FileName to write");
    this->SetErrorCode(vtkErrorCode::NoFileNameError);
    return SV_ERROR;
  }

  for (int i=0; i<numInputs; i++)
  {
    vtkSVNURBSObject *obj = vtkSVNURBSObject::GetData(inputVector[i], 0);
    if (!strncmp(obj->GetType().c_str(), "Volume", 6))
    {
      std::string fn;
      if (numInputs == 1)
        fn = this->FileName;
      else
      {
        std::string pathName = vtkSVIOUtils::GetPath(this->FileName);
        std::string rawName  = vtkSVIOUtils::GetRawName(this->FileName);
        std::string extName  = vtkSVIOUtils::GetExt(this->FileName);

        fn = pathName + "/" + rawName + "_" + vtkSVIOUtils::IntToString(i) + "." + extName;
      }

      vtkNew(vtkSVPERIGEENURBSWriter, objWriter);
      objWriter->SetInputData(obj);
      objWriter->SetFileName(fn.c_str());
      objWriter->Write();
    }
  }

  if (this->InternalCollection->GetNumberOfPatchConnections() > 0)
  {
    std::string pathName = vtkSVIOUtils::GetPath(this->FileName);
    std::string rawName  = vtkSVIOUtils::GetRawName(this->FileName);
    std::string extName  = vtkSVIOUtils::GetExt(this->FileName);

    std::string fn = pathName + "/" + rawName + "_patch_connections." + extName;

    FILE *fp;

    if ((fp = fopen(fn.c_str(), "w")) == NULL)
    {
      vtkErrorMacro(<< "Couldn't open file: " << this->FileName);
      this->SetErrorCode(vtkErrorCode::CannotOpenFileError);
      return SV_ERROR;
    }

    int numConnections = this->InternalCollection->GetNumberOfPatchConnections();
    std::vector<std::vector<int> > patchConnections = this->InternalCollection->GetPatchConnections();
    std::vector<std::vector<int> > patchFaceConnections = this->InternalCollection->GetPatchFaceConnections();
    fprintf(fp, "%d\n", numConnections );

    for (int i=0; i<numConnections; i++)
      fprintf(fp, "%d %d %d %d %d %d\n", patchConnections[i][0], patchConnections[i][1],
                                         patchFaceConnections[i][0], patchFaceConnections[i][1], 1, 1);

    if (fflush(fp))
    {
      fclose(fp);
      this->SetErrorCode(vtkErrorCode::OutOfDiskSpaceError);
      return SV_ERROR;
    }
    fclose(fp);

  }

  return SV_OK;
}


//----------------------------------------------------------------------------
void vtkSVPERIGEENURBSCollectionWriter::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "FileName: "
     << ((this->GetFileName() == NULL) ?
         "(none)" : this->GetFileName()) << std::endl;
}

//----------------------------------------------------------------------------
int vtkSVPERIGEENURBSCollectionWriter::FillInputPortInformation(int, vtkInformation *info)
{
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkSVNURBSObject");
  return 1;
}
