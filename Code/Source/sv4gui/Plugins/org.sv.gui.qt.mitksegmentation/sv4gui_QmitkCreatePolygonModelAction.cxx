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
 
/*===================================================================

The Medical Imaging Interaction Toolkit (MITK)

Copyright (c) German Cancer Research Center,
Division of Medical and Biological Informatics.
All rights reserved.

This software is distributed WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

See LICENSE.txt or http://www.mitk.org for details.

===================================================================*/
#include "sv4gui_QmitkCreatePolygonModelAction.h"

// MITK
#include <mitkShowSegmentationAsSmoothedSurface.h>
#include <mitkShowSegmentationAsSurface.h>
#include <mitkManualSegmentationToSurfaceFilter.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkDataNode.h>
#include <vtkPolyDataNormals.h>
#include <mitkIRenderWindowPart.h>
#include <mitkIRenderingManager.h>

// Blueberry
#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>
#include <berryIWorkbenchPage.h>
#include <QInputDialog>
#include <QMessageBox>

// VTK
#include <vtkPolyDataNormals.h>

// SV
#include <sv4gui_Seg3D.h>
#include <sv4gui_MitkSeg3D.h>

using namespace berry;
using namespace mitk;
using namespace std;

QmitkCreatePolygonModelAction::QmitkCreatePolygonModelAction()
{
}

QmitkCreatePolygonModelAction::~QmitkCreatePolygonModelAction()
{
}

Surface::Pointer ConvertBinaryImageToSurface(ShowSegmentationAsSurface::Pointer surfaceFilter, Image::Pointer binaryImage, double reductionRate=0.8, double gaussianSD=1.5, bool IsSmoothed=false)
{
    bool smooth = true;
    surfaceFilter->GetParameter("Smooth", smooth);

    bool applyMedian = true;
    surfaceFilter->GetParameter("Apply median", applyMedian);

    bool decimateMesh = true;
    surfaceFilter->GetParameter("Decimate mesh", decimateMesh);

    unsigned int medianKernelSize = 3;
    surfaceFilter->GetParameter("Median kernel size", medianKernelSize);

    auto filter = ManualSegmentationToSurfaceFilter::New();
    filter->SetInput(binaryImage);
    filter->SetThreshold(0.5);
    filter->SetUseGaussianImageSmooth(smooth);
    filter->SetSmooth(smooth);
    filter->SetMedianFilter3D(applyMedian);

    if (smooth)
    {
      filter->InterpolationOn();
      filter->SetGaussianStandardDeviation(gaussianSD);
    }

    if (applyMedian)
      filter->SetMedianKernelSize(medianKernelSize, medianKernelSize, medianKernelSize);
    // Fix to avoid VTK warnings (see T5390)
    if (binaryImage->GetDimension() > 3)
      decimateMesh = false;

    if (decimateMesh)
    {
      filter->SetDecimate(ImageToSurfaceFilter::QuadricDecimation);
      filter->SetTargetReduction(reductionRate);
    }
    else
    {
      filter->SetDecimate(ImageToSurfaceFilter::NoDecimation);
    }
    filter->UpdateLargestPossibleRegion();

    auto surface = filter->GetOutput();
    auto polyData = surface->GetVtkPolyData();

    if (nullptr == polyData)
      throw std::logic_error("Could not create polygon model");

    polyData->SetVerts(nullptr);
    polyData->SetLines(nullptr);
    if (smooth || applyMedian || decimateMesh)
    {
      auto normals = vtkSmartPointer<vtkPolyDataNormals>::New();

      normals->AutoOrientNormalsOn();
      normals->FlipNormalsOff();
      normals->SetInputData(polyData);

      normals->Update();

      surface->SetVtkPolyData(normals->GetOutput());
    }
    else
    {
      surface->SetVtkPolyData(polyData);
    }
    return surface;
}

void QmitkCreatePolygonModelAction::Run(const QList<DataNode::Pointer> &selectedNodes)
{
  DataNode::Pointer selectedNode = selectedNodes[0];
  Image::Pointer image = dynamic_cast<mitk::Image *>(selectedNode->GetData());

  if (image.IsNull())
  {
    return;
  }

  try
  {
    // Get preference properties for smoothing and decimation
    IPreferencesService* prefService = Platform::GetPreferencesService();
    IPreferences::Pointer segPref = prefService->GetSystemPreferences()->Node("/org.sv.views.mitksegmentation");

    bool smoothingHint = segPref->GetBool("smoothing hint", true);
    ScalarType smoothing = segPref->GetDouble("smoothing value", 1.0);
    ScalarType decimation = segPref->GetDouble("decimation rate", 0.5);

    if (smoothingHint)
    {
      smoothing = 0.0;
      Vector3D spacing = image->GetGeometry()->GetSpacing();

      for (Vector3D::Iterator iter = spacing.Begin(); iter != spacing.End(); ++iter)
        smoothing = max(smoothing, *iter);
    }

    ShowSegmentationAsSurface::Pointer surfaceFilter = ShowSegmentationAsSurface::New();

    // Activate callback functions
    itk::SimpleMemberCommand<QmitkCreatePolygonModelAction>::Pointer successCommand = itk::SimpleMemberCommand<QmitkCreatePolygonModelAction>::New();
    successCommand->SetCallbackFunction(this, &QmitkCreatePolygonModelAction::OnSurfaceCalculationDone);
    surfaceFilter->AddObserver(ResultAvailable(), successCommand);

    itk::SimpleMemberCommand<QmitkCreatePolygonModelAction>::Pointer errorCommand = itk::SimpleMemberCommand<QmitkCreatePolygonModelAction>::New();
    errorCommand->SetCallbackFunction(this, &QmitkCreatePolygonModelAction::OnSurfaceCalculationDone);
    surfaceFilter->AddObserver(ProcessingError(), errorCommand);

    // set filter parameter
    surfaceFilter->SetDataStorage(*m_DataStorage);
    surfaceFilter->SetPointerParameter("Input", image);
    //surfaceFilter->SetPointerParameter("Group node", selectedNode);
    //surfaceFilter->SetParameter("Show result", true);
    //surfaceFilter->SetParameter("Sync visibility", false);
    surfaceFilter->SetParameter("Median kernel size", 3u);
    surfaceFilter->SetParameter("Decimate mesh", m_IsDecimated);
    surfaceFilter->SetParameter("Decimation rate", (float) decimation);

    if (m_IsSmoothed)
    {
      surfaceFilter->SetParameter("Apply median", true);
      surfaceFilter->SetParameter("Smooth", true);
      surfaceFilter->SetParameter("Gaussian SD", sqrtf(smoothing)); // use sqrt to account for setting of variance in preferences
      StatusBar::GetInstance()->DisplayText("Smoothed surface creation started in background...");
    }
    else
    {
      surfaceFilter->SetParameter("Apply median", false);
      surfaceFilter->SetParameter("Smooth", false);
      StatusBar::GetInstance()->DisplayText("Surface creation started in background...");
    }

    surfaceFilter->StartAlgorithm();
    Surface::Pointer mitksurf = ConvertBinaryImageToSurface(surfaceFilter, image, decimation, sqrtf(smoothing));
    vtkSmartPointer<vtkPolyData> vtkPd = mitksurf->GetVtkPolyData();
    
    bool ok;
    QString new_polydata_name = QInputDialog::getText(0, tr("New 3D Segmentation Name"),
                                        tr("Enter a name for the new 3D Segmentation"), QLineEdit::Normal,
                                        "", &ok);
    
    mitk::DataNode::Pointer polydata_folder_node = m_DataStorage->GetNamedNode("Segmentations");
    
    if (!polydata_folder_node){
        MITK_ERROR << "No image folder found\n";
        return;
    }
    
    mitk::DataNode::Pointer newPdNode =
        m_DataStorage->GetNamedNode(new_polydata_name.toStdString());
    
    if (newPdNode){
        QMessageBox::warning(NULL,"Segmentation Already exists","Please use a different segmentation name!");
        return;
    }
    if(!ok){
        return;
    }
    
    newPdNode = mitk::DataNode::New();
    
    newPdNode->SetName(new_polydata_name.toStdString());
    
    sv4guiSeg3D* newSeg3D = new sv4guiSeg3D();
    newSeg3D->SetVtkPolyData(vtkPd);
    
    sv4guiMitkSeg3D::Pointer mitkSeg3D = sv4guiMitkSeg3D::New();
    mitkSeg3D->SetSeg3D(newSeg3D);
    mitkSeg3D->SetDataModified();
    
    newPdNode->SetData(mitkSeg3D);
    
    m_DataStorage->Add(newPdNode, polydata_folder_node);
  }
  catch(char *excp)
  {
    MITK_ERROR << "Surface creation failed! " << excp;
  }
  

}

void QmitkCreatePolygonModelAction::OnSurfaceCalculationDone()
{
  StatusBar::GetInstance()->Clear();
}

void QmitkCreatePolygonModelAction::SetDataStorage(DataStorage *dataStorage)
{
  m_DataStorage = dataStorage;
}

void QmitkCreatePolygonModelAction::SetSmoothed(bool smoothed)
{
  m_IsSmoothed = smoothed;
}

void QmitkCreatePolygonModelAction::SetDecimated(bool decimated)
{
  m_IsDecimated = decimated;
}

void QmitkCreatePolygonModelAction::SetFunctionality(QtViewPart *)
{
}

